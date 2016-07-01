{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This version attaches a session/region parameter to Iso's, and
-- let's them be used in ST.
--
-- It is a modification of version 02.

module IsoST03
      (toIsoCompact', modifyIsoVec, destroyIso, createIsoVec,
       createIsoRef)
   where

import Control.DeepSeq
import Control.Exception
-- import Data.STRef
import GHC.STRef -- expose constructor
import Data.IORef
import GHC.ST
import Control.Concurrent.MVar
import Control.Monad.Primitive
import Data.Coerce (coerce)

import Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST.Unsafe
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | This is simply a memory reference (MVar) which contains the SOLE
-- pointer to the attached data structure.  If the data structure is
-- mutable, this signifies transitive ownership over reachable mutable
-- locations.
newtype Iso s a = MkIso (MVar a)

-- | Speculate that we are the first/only reader and destructively
-- empty the Iso.
destroyIso :: Iso s a -> ST s a
destroyIso (MkIso m) = do 
  x <- unsafeIOToST $ tryTakeMVar m
  case x of
    Nothing -> error "destroyIso: iso was empty"
    Just x  -> return x

-- Sadly, we can't safely do this without a way to check that (1) the
-- input is a thunk and (2) there are no freevars in the pure expr.
unsafeFreshIso :: a -> IO (Iso s a)
unsafeFreshIso x = -- unsafePerformIO $
   do -- evaluate (rnf x)
      mv <- newMVar x
      return $ MkIso mv

-- unsafeWithMVar :: MVar a -> (a -> b) -> b
-- unsafeWithMVar mv fn = unsafePerformIO $ do
  -- x  <- takeMVar mv
  -- return $! fn x

-- Is this safe?  
instance Functor (Iso s) where
  fmap fn i = unsafePerformIO $ do
    a <- unsafeSTToIO $ destroyIso i
    unsafeFreshIso $ fn a

-- | This one would just "getCompact" before returning.  You'd
-- think that it would be definable in terms of toIsoCompact?
toIsoCompact' :: (NFData a) => a -> ST s (Iso s a)
toIsoCompact' a = unsafeIOToST $ 
   do evaluate (rnf a) -- TODO: append to compact and then extract pointer.
      mv <- newMVar a
      return $ MkIso mv

-- Pairs
--------------------------------------------------------------------------------


-- This is safe only if we already know that a/b are alias free.
-- 
unsafeUnpair :: Iso s (a,b) -> ST s (Iso s a, Iso s b)
unsafeUnpair = undefined

-- Should AliasFreeIso be a different type?


-- Referencesa
--------------------------------------------------------------------------------

unsafeCastSTRef :: STRef s a -> STRef t a 
unsafeCastSTRef s = unsafeCoerce s

createIsoRef :: (forall s. ST s (STRef s a)) -> Iso t (STRef t a)
createIsoRef s = unsafePerformIO $ do -- Could use dupable?
   ref <- unsafeSTToIO s   
   unsafeFreshIso $ unsafeCastSTRef ref

newIsoRef :: a -> (Iso t (STRef t a))
newIsoRef a = createIsoRef (newSTRef a)

-- Vectors
--------------------------------------------------------------------------------

-- | It's safe to mutate a pure vector if we're the only ones to see
-- it happen.
modifyIsoVec :: Iso t (Vector a) -> (forall s . MVector s a -> ST s ()) -> Iso t (Vector a)
modifyIsoVec iso fn = unsafePerformIO $ do
  vec  <- unsafeSTToIO $ destroyIso iso
  vec' <- unsafeThaw vec
  unsafeSTToIO (fn vec')
  mv2 <- newMVar vec
  return $! MkIso mv2

-- | Identitical to Data.Vector.create with a different return type.
createIsoVec :: (forall s. ST s (MVector s a)) -> Iso t (Vector a)
createIsoVec s =
  runST $
    do let v = V.create s 
       mv <- unsafeIOToST (newMVar v)
       return (MkIso mv)

-- Alias free parallel vectors!
splitVec :: Iso s (Vector a) -> ST s (Iso s (Vector a), Iso s (Vector a))
splitVec = undefined

splitMVec :: forall s a . Iso s (MVector s a)
          -> ST s (Iso s (MVector s a), Iso s (MVector s a))
splitMVec iso = unsafeUnpair it
  where 
  it :: Iso s (MVector s a, MVector s a)
  it = fmap (MV.splitAt 5 ) iso -- FIXME: use half of length.

-- Parallelism
--------------------------------------------------------------------------------

-- A fake parallelism monad.
newtype Par s a = Par (IO a)
 deriving (Functor, Applicative, Monad)

fork :: Par s a -> Par s a
fork = undefined

runPar :: (forall s . Par s a) -> a 
runPar = undefined

-- We cannot directly "lift" ST into Par... that would introduce races.
-- But we can do this:

-- | Perform ST computation on a (dynamically) isolated value.  Only a
-- single thread may consume such a value.
--
-- This version doesn't work very well.
withIso0 :: Iso t a -> (forall s . Iso s a -> ST s b) -> Par t b
withIso0 = undefined

-- | Higher kinded version.
withIso :: Iso t (a t) -> (forall s . Iso s (a s) -> ST s (b s)) -> Par t (b t)
withIso = undefined

-- Tests
--------------------------------------------------------------------------------

plink :: Int -> MVector s Int -> ST s ()
plink i v = MV.write v i  99

test :: ST s String
test = do v1 <- toIsoCompact' $ V.fromList [1..10::Int]
          let 
              v2 = modifyIsoVec v1 (plink 0)
              v3 = modifyIsoVec v2 (plink 5)
          v4 <- destroyIso v3
          -- v4 <- destroyIso v2 -- Deterministic exception.
          return $ show v4

-- t :: (Int,Int)
t :: Int
t = runST $ do
  tmp <- toIsoCompact' (99::Int)
  let x = fmap (+1)  tmp
      y = fmap (+10) tmp
  x' <- destroyIso x
  -- y' <- destroyIso y -- Deterministic failure.
  -- return (x',y')
  return x'


----------------------------------------
-- Higher kinded tests (painful):
----------------------------------------

newtype MyRef a s = MyRef (STRef s a)
newtype MyInt s = MyInt Int

newtype MyVec a s = MyVec (MVector s a)
data VecPair a s = VecPair (MVector s a) (MVector s a)

-- Ugly version
splitVecPair :: Iso s (MyVec a s) -> ST s (Iso s (VecPair a s))
splitVecPair iso = undefined
 where
  _ = withIso iso $ \ iso' -> do
        MyVec mv <- destroyIso iso'
        let (v1,v2) = MV.splitAt 5 mv
            x = VecPair v1 v2
        -- TODO: Need a way to flip the args on Iso itself...
        return undefined


p :: Int
p = runPar $
           do let r1 = fmap MyRef (newIsoRef (3::Int))
                  go = withIso r1 $ \ r2 -> do
                         MyRef r3 <- destroyIso r2
                         n <- readSTRef r3
                         writeSTRef r3 (n+1)
                         return (MyInt (n+1))
              fork go
              MyInt n <- go
              return n
