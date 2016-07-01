{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, RankNTypes #-}

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


-- References
--------------------------------------------------------------------------------

unsafeCastSTRef :: STRef s a -> STRef t a 
unsafeCastSTRef s = unsafeCoerce s

createIsoRef :: (forall s. ST s (STRef s a)) -> Iso t (STRef t a)
createIsoRef s = unsafePerformIO $ do -- Could use dupable?
   ref <- unsafeSTToIO s   
   unsafeFreshIso $ unsafeCastSTRef ref


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


