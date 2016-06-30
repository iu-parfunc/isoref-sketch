{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, RankNTypes #-}

-- | 

module IsoVec where

import Control.DeepSeq
import Control.Exception
import Data.STRef
import Data.IORef
import GHC.ST
import Control.Concurrent.MVar
import Control.Monad.Primitive
import Data.Coerce (coerce)

import Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Control.Monad.ST.Unsafe
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- This is simply a memory reference (MVar) which contains the SOLE
-- pointer to the attached data structure.  If the data structure is
-- mutable, this signifies transitive ownership over reachable mutable
-- locations.
newtype Iso a = MkIso (MVar a)

destroyIso :: Iso a -> IO a
destroyIso (MkIso m) = do 
  x <- tryTakeMVar m
  case x of
    Nothing -> error "destroyIso: iso was empty"
    Just x  -> return x

{-
-- References:
--------------------------------------------------------------------------------

 -- We don't want to physically copy back and forth between
 -- IORef/MVar, so one option is to use MVar for both:
data Ref (x :: Mode) s a = MkRef (MVar a)

data Mode = RD | WR | IMM

-- Could htis work for arbitrary Monad m?
toIso :: (forall s . ST s (Ref x s a)) ->  ST t (Iso a)
-- Here we can assert that it is safe to liberate the Ref:
-- toIso st = case runST (coerce st) of
--             (MkRef m) -> return (MkIso m)

-- But, an even simpler way to do it is this:
toIso st = return $! MkIso $! (runST (do MkRef m <- st; return m))


-- | Consume the Iso and return something that can directly be
-- operated on.
fromIso :: Iso a -> ST s (Ref x s a)
fromIso (MkIso m) = return (MkRef m)

-- Is this safe?  We would need the invariant that the Iso never stays
-- empty.
-- instance Functor Iso where
--  fmap (MkIso m) = unsafePerformIO (takeMVar )


readRef :: Ref RD s a -> ST s a
readRef = undefined

writeRef :: Ref WR s a -> a -> ST s ()
writeRef = undefined
-}

-- Vectors:
--------------------------------------------------------------------------------

-- -- | A non-aliased pointer to an immutable vector can be consumed to
-- -- create a mutable vector in O(1).
-- safeThaw :: Iso (Vector a) -> (MVector s a)
-- safeThaw (MkIso mv) = unsafePerformIO $ do
--     vec  <- takeMVar mv
--     unsafeThaw vec
-- safeThaw :: PrimMonad m => Iso (Vector a) -> m (MVector (PrimState m) a)
-- safeThaw  (MkIso mv) = coerce $ unsafePrimToPrim $ do vec  <- takeMVar mv
--                                                       unsafeThaw vec

-- | It's safe to mutate a pure vector if we're the only ones to see
-- it happen.
modifyIsoVec :: Iso (Vector a) -> (forall s . MVector s a -> ST s ()) -> Iso (Vector a)
modifyIsoVec (MkIso mv) fn = unsafePerformIO $ do
  vec  <- takeMVar mv
  vec' <- unsafeThaw vec
  unsafeSTToIO (fn vec')
  mv2 <- newMVar vec
  return $! MkIso mv2


-- Sadly, we can't safely do this without a way to check that there
-- are no freevars in the pure expr:
unsafeFreshIso :: NFData a => a -> Iso a
unsafeFreshIso x = unsafePerformIO $
   do evaluate (rnf x)
      mv <- newMVar x
      return $ MkIso mv

-- Fake compact for now:
newtype Compact a = Compact a
getCompact (Compact a) = a

-- Build an alias-free version of a data structure by creating a
-- fresh copy.
toIsoCompact :: (PrimMonad m, NFData a) => a -> m (Iso (Compact a))
toIsoCompact a = 
  rnf a `seq`
   (return $ MkIso $ unsafePerformIO $ newMVar (Compact a))

-- This one would just "getCompact" first:
toIsoCompact' :: (NFData a) => a -> IO (Iso a)
toIsoCompact' a =
   do evaluate (rnf a) -- TODO: append to compact and then extract pointer.
      mv <- newMVar a
      return $ MkIso mv

-- We can really only safely create fresh Iso's for MUTABLE types.
-- Maybe we need a class here.

plink :: Int -> MVector s Int -> ST s ()
plink i v = VM.write v i  99

test = do v1 <- toIsoCompact' $ V.fromList [1..10::Int]
          let 
              v2 = modifyIsoVec v1 (plink 0)
              v3 = modifyIsoVec v2 (plink 5)
          evaluate v3 -- monkey with it.
          v4 <- destroyIso v3
          putStrLn $ "test: "
          print v4

-- Example client code (Safe) 
--------------------------------------------------------------------------------
{-
add1 :: Iso Int -> Iso Int
add1 i = runST $ toIso $ do
   i'  <- fromIso i
   i'' <- readRef i'
--   writeRef i' 3  -- Can't do both...
   let new = i'' + 1
   -- Now we need
   return (MkRef undefined)
-}
