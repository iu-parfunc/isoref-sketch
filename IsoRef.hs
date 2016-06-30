{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, RankNTypes #-}

-- | 

module IsoRef where

import Data.STRef
import GHC.ST
import Control.Concurrent.MVar
-- import Data.Coerce (coerce)

--------------------------------------------------------------------------------

newtype Iso a = MkIso (MVar a)

 -- We don't want to physically copy back and forth between
 -- IORef/MVar, so one option is to use MVar for both:
data Ref (x :: Mode) s a = MkRef (MVar a)

data Mode = RD | WR | IMM

--------------------------------------------------------------------------------

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
