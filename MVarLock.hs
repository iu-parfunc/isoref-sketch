{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- | 

module MVarLock where

import Control.Concurrent.MVar
-- import GHC.Conc

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.STRef

--------------------------------------------------------------------------------

-- | A lock which, when held, grants the authority to modify other
-- pieces of state such as STRefs.
data MVarLock s a = MVarLock (MVar a)

-- | New MVar Lock.  Unsafe because it doesn't ensure that there's a unique 's' per
-- MVarLock.  I.e. two MVarLock's can share the same 's'.
unsafeNewMVarLock :: ST s (a,b) -> IO (MVarLock s a, b)
unsafeNewMVarLock = undefined

-- | Create a new MVarLock and construct a state that contains it,
--  while existentially quantifying its region parameter.
--
--  This is safe for the usual reason.  `s` can't escape except in
--  existial bindings where it can do no harm.  No second MVarLock can
--  be typed with a compatible `s`.
newMVarLock :: a -> (forall s . MVarLock s a -> ST s b) -> IO b
newMVarLock init fn =
  do mv <- newMVar init
     stToIO (fn (MVarLock mv))

-- | Use the lock to operate on the contained state, and update the
-- value in the MVar.
withMVarLock :: MVarLock s a
             -> (a -> ST s (a, b))
             -> IO b
withMVarLock (MVarLock mv) fn =
  modifyMVar mv (unsafeSTToIO . fn)
       
-- | Never blocks because an MVarLock is always FULL (unlike a normal MVar).
readMVarLock :: MVarLock s a -> IO a
readMVarLock (MVarLock m) = readMVar m

-- | A version which does not update the MVar state and may save
-- allocating a result tuple, which may happen anyway with CPR/demand analysis.
withMVarLock_ :: MVarLock s a -> (a -> ST s b) -> IO b
withMVarLock_ = undefined


--------------------------------------------------------------------------------
-- Example datatype and usage:
--------------------------------------------------------------------------------

data MyRec = forall s . MyRec (MVarLock s Bool) (STRef s Int)

unsafeTest00 :: IO MyRec
unsafeTest00 =
  do (mv,r) <- unsafeNewMVarLock (do r <- newSTRef 3; return (True, r))                             
     return $ MyRec mv r

-- | Here we close over the 's' param in our existential MyRec constructor:
safeTest00 :: IO (Bool,Int,Int,Int)
safeTest00 = do
  mr@(MyRec m _) <- newMVarLock True $ \mv -> do
                     r <- newSTRef 3
                     return $! MyRec mv r 
  x <- modRec mr
  y <- modRec mr
  z <- modRec mr
  b <- readMVarLock m
  return (b,x,y,z)
        
-- | Open up the existential package and use the lock to modify the field:
modRec :: MyRec -> IO Int
modRec (MyRec mv r) =
  withMVarLock mv $ \ b -> do
    n <- readSTRef r
    writeSTRef r $! (n+1)
    return (not b, (n+1))


--------------------------------------------------------------------------------
-- SCRAP: Experimenting
--------------------------------------------------------------------------------

-- This explores the idea of using a higher rank type and then
-- "teleport" the 'a' result out of it.  Don't let the b result have
-- any mutable bits.

-- But this was just an experiment.  We don't actually want the
-- MVarLock to have some `t` type that could unify with the type of
-- other MVar locks.  This retains the same problem as the
-- unsafeNewMVarLock, but it explores a trick that may be useful
-- elsewhere.

newMVarLock' :: (STCastable a, STCastable b) =>
-- Option 1: attempt to cast outputs (skolem var errors)
--                (forall s . ST s (a,b)) -> IO (MVarLock t a, (CastState t b))
-- Option 2: attempt to cast inputs:
               (forall s . ST s (a, CastState s b)) -> IO (MVarLock t a, b)
newMVarLock' = undefined

-- This succeeds in casting the bound `s` to a `t`.
test00 :: IO MyRec
test00 =
  do (mv,r) <- newMVarLock' (do r <- newSTRef 3; return (True, r))
     return $ MyRec mv r

-- NOTE: This kind of type class is difficult to use, because, if a
-- rigid type variable is INPUT, you can get an an error before the
-- type family application reduces and discards it.

class STCastable (a :: *) where
  -- What is the 's' param of this type:
  type GetState a :: *
  -- OPTION: Could have `CastState s t a` with both the source and target.
  -- May be helpful for avoiding ambiguity.
  type CastState s a :: * -- = res | res -> s

--  cast :: a -> CastState t a

instance STCastable a => STCastable (STRef s a) where
  type GetState (STRef s a)    = s
  type CastState t (STRef s a) = STRef t (CastState t a)

-- Should be derivable:
instance STCastable Int where
  type GetState Int    = ()
  type CastState t Int = Int

instance STCastable Bool where
  type GetState Bool    = ()
  type CastState t Bool = Bool


--------------------------------------------------------------------------------
