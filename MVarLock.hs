{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- | 

module MVarLock
    ( MVarLock -- Must stay abstract.
    , newMVarLock
    , newEmptyMVarLock
    , newEmptyMVarLock7
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
newEmptyMVarLock :: (forall s . MVarLock s a -> ST s (a,b)) -> IO b
newEmptyMVarLock fn =
  do mv <- newEmptyMVar
     (a,b) <- stToIO (fn (MVarLock mv))
     putMVar mv a
     return b

----------------------------------------
            
-- This variant takes an explicit setter function to write the MVar.
-- It has problems though...
newEmptyMVarLock2 :: (forall s . MVarLock s a -> (a -> ST s ()) -> ST s b) -> IO b
newEmptyMVarLock2 fn =
  do mv <- newEmptyMVar
     stToIO (fn (MVarLock mv) (unsafeIOToST . putMVar mv))

----------------------------------------

newEmptyMVarLock3 :: STCastable a => (forall s . MVarLock s a -> (CastState () a -> ST s ()) -> ST s b) -> IO b
newEmptyMVarLock3 = undefined
{- 
Again, Casting on the input to the fill function will result in in error like this:

    VarLock.hs:126:15: Couldn't match expected type ‘CastState () a1’ …
                    with actual type ‘STRef s a0’
        The type variables ‘a0’, ‘a1’ are ambiguous
    MVarLock.hs:127:25: Couldn't match type ‘a1’ with ‘STRef s Int’ …
          because type variable ‘s’ would escape its scope
in:
    safeTest02 = do
      mr <- newEmptyMVarLock3 $ \mv fill -> do
             r <- newSTRef 3
             fill r 
             return (MyRec2 mv)
      undefined

We can't unify a1... even though if we did, CastState would reduce to eliminate the offending 's'.
-}

----------------------------------------

newEmptyMVarLock4 :: STCastable a => (forall s . MVarLock s a -> (forall t . CastState t a -> ST t ()) -> ST s b) -> IO b
newEmptyMVarLock4 = undefined

safeTest04 :: IO ()
safeTest04 = do
  let fn :: forall s . MVarLock s (STRef s Int) -> (forall t . CastState t (STRef s Int) -> ST t ()) -> ST s MyRec2
      fn mv fill = do r <- newSTRef (3::Int)
           --         fill r 
                      return (MyRec2 mv)
  -- Still won't work; the type error comes HERE:  
  -- mr <- newEmptyMVarLock4 fn
  -- That is, as soon as we try to unify the larger-scoped `a` with
  -- `STRef s`, we're in trouble.
  return ()

----------------------------------------
         
-- Ok, here let's try to not mention s in any type scoped outside s's binding.
-- Rather, let's cast to 's' on demand:
newEmptyMVarLock5 :: STCastable a
                  => Proxy a 
                  -> (forall s . MVarLock s (CastState s a) -> (CastState s a -> ST s ()) -> ST s b)
                  -> IO b
newEmptyMVarLock5 Proxy fn =
  do mv <- newEmptyMVar
     stToIO (fn (MVarLock mv) (unsafeIOToST . putMVar mv))

                    
-- | This one works.  But it's far from ergonomic.
safeTest05 :: IO Int
safeTest05 = do
  let fn :: forall s . MVarLock s (CastState s (STRef () Int))
         -> (CastState s (STRef () Int) -> ST s ()) -> ST s MyRec2
      fn mv fill = do r <- newSTRef (3::Int)
                      fill r 
                      return (MyRec2 mv)
  MyRec2 mr <- newEmptyMVarLock5 (Proxy::Proxy (STRef () Int)) fn
  withMVarLock_ mr readSTRef  
  
-- | Ok, well it doesn't need to be quite *that* verbose.
--   But we do need the proxy to avoid ambiguity.
safeTest05' :: IO Int
safeTest05' = do
  MyRec2 mr <- newEmptyMVarLock5 (Proxy::Proxy (STRef () Int)) $ \ mv fill -> do 
                 r <- newSTRef (3::Int)
                 fill r 
                 return (MyRec2 mv)
  withMVarLock_ mr readSTRef  

                
----------------------------------------

-- Can we avoid the proxy by fixing the type we cast from?
-- Maybe if we use injectivity annotations...
-- newEmptyMVarLock6 :: STCastable2 a
--                   => (forall s . MVarLock s (CastState2 () s a) -> (CastState2 () s a -> ST s ()) -> ST s b)
--                   -> IO b


-- Last, let's try to go back to returning the value 
-- rather than calling a fill function.
----------------------------------------

-- | This function is useful for constructing MVarLocks with internally MUTABLE state.
--   Doing this requires a bit of a dance to avoid the 's' variable escaping its scope.
-- 
--   Namely, it `s` cannot be inside a type unified with `a`, even if
--   a type family will subsequently discard it.
newEmptyMVarLock7 :: STCastable a
                  => Proxy a 
                  -> (forall s . MVarLock s (CastState s a) -> ST s (CastState s a, b))
                  -> IO b
newEmptyMVarLock7 Proxy fn =
  do mv <- newEmptyMVar
     (a,x) <- stToIO (fn (MVarLock mv))
     putMVar mv a
     return x

-- | This is both more ergonomic, and safer, because the user cannot
-- forget to fill the MVar.
safeTest07 :: IO Int
safeTest07 = do
  MyRec2 mr <- newEmptyMVarLock7 (Proxy::Proxy (STRef () Int)) $ \mv -> do
                 r <- newSTRef (3::Int)
                 return (r, MyRec2 mv)
  withMVarLock_ mr readSTRef  


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

-- | In this example, we make the state internal rather than outside the MVar:
data MyRec2 = forall s . MyRec2 (MVarLock s (STRef s Int))
-- FIXME ^ No good way to build this right now.

-- This can't work with newMVarLock or newEmptyMVarLock:
{-
safeTest01 = do
  mr <- newEmptyMVarLock $ \mv -> do
         r <- newSTRef 3
         return (r, MyRec2 mv)
--  withMVarLock mr
  undefined
-}




--------------------------------------------------------------------------------
-- SCRAP: Experimenting
--------------------------------------------------------------------------------

-- This explores the idea of using a higher rank type and then
-- "teleport" the a result out of it by using a Cast.

-- But this was just an experiment.  We don't actually want the
-- MVarLock to have some `t` type that could unify with the type of
-- other MVar locks.  This retains the same problem as the
-- unsafeNewMVarLock, but it explores a trick that may be useful
-- elsewhere.

newMVarLock' :: (STCastable a, STCastable b) =>
-- Option 1: attempt to cast outputs (skolem var errors)
--                (forall s . ST s (a,b)) -> IO (MVarLock t a, (CastState t b))
-- Option 2: attempt to cast intermediate result:
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
