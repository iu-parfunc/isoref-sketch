{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- | 

module MVarLock
    ( MVarLock -- Must stay abstract.
    , newMVarLock
    , newMVarLock' 
    , newMVarLockMutable
    , STCastable() -- The user cannot invoke these casts!
    )
    where

import Control.Concurrent.MVar
-- import GHC.Conc

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeSTToIO, unsafeIOToST)
import Data.STRef
import Data.Proxy
    
--------------------------------------------------------------------------------

-- | A lock which, when held, grants the authority to modify other
-- pieces of state such as STRefs.
data MVarLock s a = MVarLock (MVar a)

-- | New MVar Lock.  Unsafe because it doesn't ensure that there's a unique 's' per
-- MVarLock.  I.e. two MVarLock's can share the same 's'.
unsafeNewMVarLock :: ST s (a,b) -> IO (MVarLock s a, b)
unsafeNewMVarLock = undefined

-- | Create a new MVarLock and construct a state that contains it,
--  while existentially quantifying its region parameter.  To use this
--  function, the value inside the MVar must NOT be ST-mutable.
--
--  This is safe for the usual reason.  `s` can't escape except in
--  existial bindings where it can do no harm.  No second MVarLock can
--  be typed with a compatible `s`.
newMVarLock :: a -> (forall s . MVarLock s a -> ST s b) -> IO b
newMVarLock init fn =
  do mv <- newMVar init
     stToIO (fn (MVarLock mv))

-- | A variant of `newMVarLock` where the MVarLock starts empty, and
-- is initially filled with the `a` result of the user's computation.
--
-- Again, this version works when the state `a` is NOT ST-mutable.
newMVarLock' :: (forall s . MVarLock s a -> ST s (a,b)) -> IO b
newMVarLock' fn =
  do mv <- newEmptyMVar
     (a,b) <- stToIO (fn (MVarLock mv))
     putMVar mv a
     return b

----------------------------------------
-- Note, several experimental/intermediate versions were here, but
-- discarded in 4fe4b7441932b94f511f60586e79a2eb081c0cf9


-- | This function is useful for constructing MVarLocks with internally MUTABLE state.
--   Doing this requires a bit of a dance to avoid the 's' variable escaping its scope.
-- 
--   Namely, it `s` cannot be inside a type unified with `a`, even if
--   a type family will subsequently discard it.
newMVarLockMutable :: STCastable a
                   => Proxy a 
                   -> (forall s . MVarLock s (CastState s a) -> ST s (CastState s a, b))
                   -> IO b
newMVarLockMutable Proxy fn =
  do mv <- newEmptyMVar
     (a,x) <- stToIO (fn (MVarLock mv))
     putMVar mv a
     return x
            
--------------------------------------------------------------------------------
-- Using the locks
--------------------------------------------------------------------------------

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
withMVarLock_ mv fn =
    -- Inefficient implementation:
    withMVarLock mv (\ a -> do x <- fn a; return (a,x))

-- | Are two MVars the same memory location?
--   This is a form of heterogeneous equality which is handy when
--   dealing with existential types.
sameMVarLock :: MVarLock s a -> MVarLock t a -> Bool
sameMVarLock (MVarLock m1) (MVarLock m2) = m1 == m2

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

----------------------------------------
           
-- | In this example, we make the state internal rather than outside the MVar:
data MyRec2 = forall s . MyRec2 (MVarLock s (STRef s Int))

-- | A safe test that builds MyRec2
safeTest07 :: IO Int
safeTest07 = do
  MyRec2 mr <- newMVarLockMutable (Proxy::Proxy (STRef () Int)) $ \mv -> do
                 r <- newSTRef (3::Int)
                 return (r, MyRec2 mv)
  withMVarLock_ mr readSTRef  



--------------------------------------------------------------------------------
-- Castable
--------------------------------------------------------------------------------

-- NOTE: This kind of type class is difficult to use, because, if a
-- rigid type variable is INPUT, you can get an an error before the
-- type family application reduces and discards it.

-- | An unsafe interface for changing the region parameter associated
-- with state.
class STCastable (a :: *) where
  -- What is the 's' param of this type:
  type GetState a :: *
  -- OPTION: Could have `CastState s t a` with both the source and target.
  -- May be helpful for avoiding ambiguity.
  type CastState s a :: * -- = res | res -> s

  -- unsafeCastState :: a -> CastState t a

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

-- | In some cases it may help avoid ambiguity if we can name both the
-- source and target types as inputs.
class STCastable2 (a :: *) where
  type GetState2 a :: *
  type CastState2 s t a :: * 

instance STCastable2 a => STCastable2 (STRef s a) where
  type GetState2 (STRef s a)    = s
  type CastState2 s t (STRef s a) = STRef t (CastState2 s t a)

-- Should be derivable:
instance STCastable2 Int where
  type GetState2 Int    = ()
  type CastState2 s t Int = Int

instance STCastable2 Bool where
  type GetState2 Bool    = ()
  type CastState2 s t Bool = Bool


--------------------------------------------------------------------------------
