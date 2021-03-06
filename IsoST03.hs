{-# LANGUAGE InstanceSigs #-}
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

newtype Compact a = Compact { getCompact :: a } -- Placeholder

newtype IsoCompact s a = IsoCompact (Iso s (Compact a))

appendCompact :: NFData a => a -> IO (Compact a)
appendCompact = undefined

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

-- Is this safe?  NO!  See counterexample below.
instance Functor (Iso s) where
  fmap fn i = unsafePerformIO $ do
    a <- unsafeSTToIO $ destroyIso i
    unsafeFreshIso $ fn a
    -- This only works for the class of functions that return "all
    -- fresh" results.  Results from which all reachable heap
    -- locations were allocated since the start of the function's
    -- execution.  We can only guarantee this for Compact outputs.

-- This won't work because we can't get the NFData b context:
{- 
instance Functor (IsoCompact s) where
-}

-- This is what we can do.  This isn't that useful, except that it
-- avoids the ST monad unlike toIsoCompact.  Why is that safe?  It is
-- only safe if we don't expose the identity of the Compact blocks
-- themselves, right?

-- This doesn't feel right, actually.  I think compactContains and
-- compactContainsAny, must instead move to the IO monad.
mapIsoCompact :: (NFData b) => (a -> b) -> IsoCompact s a -> IsoCompact s b
mapIsoCompact  fn (IsoCompact i) =  unsafePerformIO $ do
    a <- unsafeSTToIO $ destroyIso i
    let b = fn (getCompact a)
    b' <- toIsoCompact b
    return (IsoCompact b')

-- Compact a value and return an isolated pointer to it:
-- 
-- This is in the ST monad to prevent duplication, and because we
-- expose equality checks (physical identity) on Compact regions.
-- 
toIsoCompact :: (NFData a) => a -> IO (Iso s (Compact a))
toIsoCompact = undefined

-- | This variant one would just "getCompact" before returning.  You'd
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

-- | A data type without internal sharing.  I.e. no
-- potentially-mutable location should be reachable by two different
-- paths through the heap structure
newtype AF a = AliasFree a

-- | Some types are always aliasfree:
class AlwaysAliasFree a where
 -- only trusted users can instance this... TODO: lock it down
 af :: a -> AF a
 af = AliasFree 

instance AlwaysAliasFree Int
instance AlwaysAliasFree Char
instance AlwaysAliasFree a => AlwaysAliasFree (MVector s a)
instance AlwaysAliasFree a => AlwaysAliasFree (Vector a)

-- | Safe unpairing of isolated, alias-free state.
unpair :: Iso s (AF (a,b))
       -> ST s (Iso s (AF a), Iso s (AF b))
unpair = undefined


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

-- FIXME: This is unsafe.  It will only work for unboxed vectors.
-- For boxed vectors there could be sharing.
--
-- How this falls out depends on the details of how we handle nested
-- mutable data within `a`.
splitMVec :: forall s a . Iso s (MVector s a)
          -> ST s (Iso s (MVector s a), Iso s (MVector s a))
splitMVec iso = unsafeUnpair it
  where 
  it :: Iso s (MVector s a, MVector s a)
  it = fmap (MV.splitAt 5 ) iso -- FIXME: use half of length.

-- Here's a version that uses explicit alias-freedom tracking.
splitMVec' :: forall s a . Iso s (AF (MVector s a))
          -> ST s (Iso s (AF (MVector s a)), Iso s (AF (MVector s a)))
splitMVec' iso = undefined



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

-- | COUNTEREXAMPLE:  Functor instance is not safe.
t2 = runST $ do
  tmp <- toIsoCompact' (99::Int)
  -- A non-isolated value that we reference elsewhere:
  let noniso = 2 + 2
  x <- destroyIso $ fmap (\_ -> noniso) tmp
  return (noniso,x) -- Second use!!


----------------------------------------
-- Higher kinded tests (painful):
----------------------------------------

-- Alternative would be STCastable type class.  But how to make it
-- safe?

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
