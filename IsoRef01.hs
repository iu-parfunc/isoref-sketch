{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, RankNTypes, GADTs #-}

-- | This initial version was based on the assumption that an Iso
-- implied "mutable ref".  It was misguided.

module IsoRef01 where

import Data.STRef
import Data.IORef
import GHC.ST
import Control.Concurrent.MVar
import Data.Coerce (coerce)
-- import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.ST.Unsafe 

--------------------------------------------------------------------------------


-- data Iso a = forall s . MkIso !(MVar (STRef s a))

newtype Iso a = MkIso (MVar (IORef a))

newtype Ref (x :: Mode) s a = MkRef (IORef a)

data Mode = RD | WR | IMM

--------------------------------------------------------------------------------

-- Could this work for arbitrary Monad m?
toIso :: (forall s . ST s (Ref x s a)) ->  ST t (Iso a)
-- toIso = undefined $! (runST (do MkRef m <- st; return m))
toIso st = unsafeIOToST $ do
  -- (do MkRef m <- st; return m))
  MkRef r <- unsafeSTToIO st
  mv <- newMVar r
  return $! MkIso $! mv

-- | Consume the Iso and return something that can directly be
-- operated on.
fromIso :: Iso a -> ST s (Ref x s a)
fromIso (MkIso m) = unsafeIOToST $ do
  -- Here we are marking that no one else can read it.
  r <- takeMVar m
  -- Going with the dynamic theme... we could even associate a
  -- finalizer with the reference such that when it is collected,
  -- the Iso's MVar is restored...
  return $! MkRef r


inIsolation :: Iso a -> (forall s . a -> ST s b) -> b
inIsolation = undefined

{-

-- Is this safe?  We would need the invariant that the Iso never stays
-- empty.
-- instance Functor Iso where
--  fmap (MkIso m) = unsafePerformIO (takeMVar )


readRef :: Ref RD s a -> ST s a
readRef = undefined

writeRef :: Ref WR s a -> a -> ST s ()
writeRef = undefined


-- Example client code (Safe) 
--------------------------------------------------------------------------------

add1 :: Iso Int -> Iso Int
add1 i = runST $ toIso $ do
   i'  <- fromIso i
   i'' <- readRef i'
--   writeRef i' 3  -- Can't do both...
   let new = i'' + 1
   -- Now we need
   return (MkRef undefined)
-}


