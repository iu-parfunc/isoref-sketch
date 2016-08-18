{-# LANGUAGE RankNTypes #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language GADTs, FunctionalDependencies #-}
{-# Language PolyKinds #-}

{-# Language RebindableSyntax #-}

-- Code for the article. The code is deliberately not general,
-- to demonstrate the problem and the solution in the starkest form.
-- The generalizations and optimizations have been described elsewhere.


module ParamEff1 (putP, runStateP) where

import GHC.Exts (BOX)
import qualified Prelude as P
import Prelude ( error, ($), fromInteger, Int, (+), show, Bool(..), (.), id
               , String, Char, not, (>), const)
  
-- Name of variables (mutable cells)
-- The names of all variables should be known statically

data Var1 = Var1; data Var2 = Var2
          
-- State monad with multiple pieces of state:
-- the simple extension of MonadState
-- We use modify as the primitive for simplicity
-- (although this is less efficient)
-- Modify takes the state transformer and applies it to
-- the current state, returning the original state
class P.Monad m => MonadMState var s m | var m -> s where
  modify :: var -> (s->s) -> m s

-- The familiar State operations are expressed as follows

get :: MonadMState var s m => var -> m s
get var = modify var id

put :: MonadMState var s m => var -> s -> m ()
put var s = modify var (const s) P.>> P.return ()
            
{-
            
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= return . f


-- The signature is inferred
ts1 :: MonadMState Var1 Char m => m Char
ts1 = do 
  x <- liftM succ $ get Var1
  put Var1 'c'
  -- put Var1 1    -- ERR: Char is not a number
  -- liftM not $ get Var1 -- ERR: Could not deduce (MonadMState Var1 Bool m)
  return x   
         
ts2 :: MonadMState Var2 Int m => m Int
ts2 = do 
  put Var2 (10::Int)
  x <- get Var2
  put Var2 20
  y <- get Var2
  return (x+y) 

-- Combining both state effects in the same program
ts3 :: (MonadMState Var1 Char m, MonadMState Var2 Int m) =>
     m (Char, Int, Char)
ts3 = do
  x <- ts1
  y <- ts2
  z <- ts1
  return (x,y,z)



-- How to build MonadMState? Use the Freer monad

-- Here, f is any *->* type constructor, without any restrictions.
-- It does not have to be a Functor
data Eff f a where
  Pure   :: a -> Eff f a
  Impure :: f x -> (x -> Eff f a) -> Eff f a

instance Functor (Eff f) where
    fmap = error "FINISHME"
instance Applicative (Eff f) where
    pure = error "FINISHME"
    (<*>) = error "FINISHME"
instance Monad (Eff f) where
  return = Pure
  Pure x       >>= k = k x
  Impure fx k1 >>= k = Impure fx (\x -> k1 x >>= k)

data Void x

run :: Eff Void a -> a
run (Pure x) = x
-- Impure is impossible here

data State var s x where
  Modify :: (s->s) -> State var s s


instance MonadMState var s (Eff (State var s)) where
  modify _ tr  = Impure (Modify tr)  Pure

runState :: var -> s -> Eff (State var s) a -> (a,s)
runState _ s (Pure x)               = (x,s)
runState v s (Impure (Modify tr) k) = runState v (tr s) (k s)


ts1r = runState Var1 'a' ts1
-- ('b','c')

ts2r = ((30,20) ==) $ runState Var2 0 ts2

-- Combining effects, in a very naive way, using Either
-- See the extensible effects papers for a better way, using
-- a properly designed union

-- Union for two *->* type constructors
data Sum f1 f2 x = L (f1 x) | R (f2 x)

instance MonadMState var s (Eff (Sum (State var s) f2)) where
  modify _ tr  = Impure (L (Modify tr))  Pure

-- We use overlapping instances, which is common from the
-- first monad transformers paper on
instance MonadMState var s (Eff (Sum f1 (State var s))) where
  modify _ tr  = Impure (R (Modify tr))  Pure


runState1 :: var -> s -> Eff (Sum (State var s) f2) a ->
             Eff f2 (a,s)
runState1 _ s (Pure x)                   = return (x,s)
runState1 v s (Impure (L (Modify tr)) k) = runState1 v (tr s) (k s)
runState1 v s (Impure (R fx) k)          = Impure fx (runState1 v s . k)

ts3r1 = runState1 Var1 'a' ts3

ts3r = runState Var2 0 $ runState1 Var1 'a' ts3

-- Type varying state

-- Clearly, monad will not work (why?)

{-
-- Infers both (MonadMState Var1 Char m) and
-- (MonadMState Var1 Bool m)
-- that is, the conjunction of the constraints.
-- The conjunction is clearly inconsistent.
-- What we need is the notion of `time': the second constraint
-- holds only after the first one.

tsx1 = do 
  put Var1 'c'
  put Var1 True
-}

-} -- End monad version

-- Does not show how m changes when state changes from s1 to s2
{-
class MonadMPState var s1 s2 m | var m -> s1 s2 where
  modifyP :: var -> (s1->s2) -> m s1
-}

-- The same problem as before
{-
getP var   = modifyP var id
putP var s = modifyP var (const s) >> return ()

tsx1 = do 
  putP Var1 'c'
  putP Var1 True
-}

{-
-- The following still doesn't work: to resolve the NextMonad constraint,
-- we need to determine n and m, but we only know p (from the type
-- of the expression's result).
-- Looks like PolyMonad, but very restricted, by a functional dependency
-- There is much less ambiguity now
class NextMonad m n p | m n -> p, m p -> n, n p -> m where
  (>>>=) :: m a -> (a -> n b) -> p b

tsP0 = putP Var1 'c' >>>= \_ ->
       putP Var1 True

-}


-- The well-known solution: parameterized monad

class Monadish m where
  (>>>=) :: m s t a -> (a -> m t u b) -> m s u b

-- Easy to generalize Eff

data EffP f s1 s2 a where
  PureP   :: a -> EffP f s s a
  ImpureP :: f s1 sx x -> (x -> EffP f sx s2 b) -> EffP f s1 s2 b

-- When two parameters are the same, Monadish is the ordinaru monad
-- (That is why we did not have a ret method in Monadish)

instance P.Functor (EffP f s s) where
    fmap = error "FINISHME"    
instance P.Applicative (EffP f s s) where
    pure = error "FINISHME"
    (<*>) = error "FINISHME"
    
instance P.Monad (EffP f s s) where
  return = PureP
  PureP x        >>= k = k x
  ImpureP fx k1  >>= k = ImpureP fx (\x -> k1 x >>>= k)

instance Monadish (EffP f) where
  PureP x        >>>= k = k x
  ImpureP fx k1  >>>= k = ImpureP fx (\x -> k1 x >>>= k)

ret :: (Monadish m, P.Monad (m s s)) => a -> m s s a
ret = P.return

-- Straightforward generalization of State

data StateP var s1 s2 x where
  ModifyP :: (s1->s2) -> StateP var s1 s2 s1

instance MonadMState var s (EffP (StateP var) s s) where
  modify _ tr = ImpureP (ModifyP tr) PureP
  
runStateP :: var -> s1 -> EffP (StateP var) s1 s2 a -> (a,s2)
runStateP _ s (PureP x)                 = (x,s)
runStateP v s (ImpureP (ModifyP tr) k)  = runStateP v (tr s) (k s)

-- tsp1 :: EffP (StateP var) Char String (Char, Bool, Bool)
tsp1 =
  ImpureP (ModifyP (> 'a')) PureP >>>= \x ->
  ImpureP (ModifyP not)  PureP >>>= \y ->
  ImpureP (ModifyP show) PureP >>>= \z ->
  PureP (x,y,z)

tsp1r = runStateP Var1 'a' tsp1
-- (('a',False,True),"True")


-- Why we no longer need the functional dependency?
-- Monad is parameterized by the state already
class Monadish m => MonadMPState1 var s1 s2 m  where
  modifyP' :: var -> (s1->s2) -> m s1 s2 s1

-- we can even define the instance

instance MonadMPState1 var s1 s2 (EffP (StateP var)) where
  modifyP' var tr = ImpureP (ModifyP tr) PureP

-- But we can't generalize two two pieces of the state
-- modifyP above assumes that the type-state (the parameters of m)
-- are the state of var. But the state of var is only a part
-- of the whole typestate, not the entire type-state.


-- How to read: as a 6-place relation
-- Or: according to the security policy m, in the type-state t1
-- one is permitted to access the state as s1 and change it to s2.
-- The type-state changes to t2.

{-
The original design was as follows. Alas, the type checker could not
use the var m t2 -> s2 dependency to improve the type of s2 when t2
was known (from the following monad in the chain). Hence we had to factor
out the dependencies

class Monadish m =>
  MonadMPState var s1 s2 t1 t2 m 
    | var m t1 -> s1, var m t1 -> s2, var m t1 s2 -> t2 where
  modifyP :: var -> (s1->s2) -> m t1 t2 s1
-}


class TSProj var s t m | var m t -> s

class Monadish m =>
      MonadMPState var s1 s2 t1 t2 m | var m t1 s2 -> t2 where
  modifyP :: (TSProj var s1 t1 m,
              TSProj var s2 t2 m) =>
             var -> (s1->s2) -> m t1 t2 s1

-- The state of var is the entire type-state
instance TSProj var s s (EffP (StateP var))

instance MonadMPState var s1 s2 s1 s2 (EffP (StateP var)) where
  modifyP var tr = ImpureP (ModifyP tr) PureP

getP :: (Monadish m, TSProj var s t m, MonadMState var s (m t t)) =>
        var -> m t t s
getP       = get
putP var s = modifyP var (const s) >>>= \_ -> ret ()

(>>=) :: Monadish m => m s t a -> (a -> m t u b) -> m s u b
(>>=) = (>>>=)

return :: (P.Monad (m s s), Monadish m) =>
          a -> m s s a
return = ret

fail :: P.Monad m => String -> m a
fail = P.fail

(>>) :: Monadish m => m s t a -> m t u b -> m s u b
m1 >> m2 = m1 >>= (\_ -> m2)

-- Session-like type!
-- tsP1 :: (TSProj Var1 Char t m, TSProj Var1 Bool u m, TSProj Var1 a s m,
--       MonadMPState Var1 Char Bool t u m, MonadMPState Var1 a Char s t m,
--       P.Monad (m u u), P.Monad (m t t)) =>
--      m s u ()
tsP1 = do putP Var1 'c' 
          putP Var1 True


tsP1r = runStateP Var1 () tsP1
-- ((),True)

tsP2
  :: (MonadMState Var1 Int (m t t), MonadMState Var2 Int (m t t),
      TSProj Var1 String u m, TSProj Var1 Int t m, TSProj Var1 a t1 m,
      TSProj Var2 Int t m, TSProj Var2 Char t1 m,
      MonadMPState Var1 a String t1 u m,
      MonadMPState Var2 Int Char t t1 m, P.Monad (m t1 t1),
      P.Monad (m u u)) =>
     m t u ()
tsP2 = do x <- getP Var1
          y <- getP Var2 
          putP Var2 'c' 
          putP Var1 (show (x+y+1::Int))

-- The following are inconsistent
{-
tsP2' =
  getP Var1 >>>= \x ->
  putP Var2 'c' >>>= \_ ->
  getP Var2 >>>= \y ->
  putP Var1 (x+y+1::Int)

-}

-- See the inferred type: session type!
tsP4 = do x0 <- getP Var1
          putP Var1 True
          x <- getP Var1
          putP Var1 'a'
          y <- getP Var1
          return (x0,x,y)

tsP4r = runStateP Var1 () tsP4
-- (((),True,'a'),'a')

-- Generalize the union
-- Union for two *->*->*->* type constructors
-- The union of two security policies

-- But this assumes that f1 and f2 share the same typestate.
-- f1 and f2 may have different type-state, and we which to combine
-- it.
-- Here is the analogy with the vector space?
-- data SumP f1 f2 s1 s2 x = LP (f1 s1 s2 x) | RP (f2 s1 s2 x)

-- Won't work either: If the change is on the left, there is no change
-- in the component
-- That is, if LP is present, sr1 must be equal to sr2
-- data SumP f1 f2 (sl1,sr1) (sl2,sr2) x =
--  LP (f1 sl1 sl2 x) | RP (f2 sr1 sr2 x)

-- That is, we need GADT

data SumP f1 f2 t1 t2 x where
  LP :: f1 sl1 sl2 x -> SumP f1 f2 (sl1,sr) (sl2,sr) x
  RP :: f2 sr1 sr2 x -> SumP f1 f2 (sl,sr1) (sl,sr2) x

runStateP1 :: var -> s1 -> EffP (SumP (StateP var) f2) (s1,t1) (s2,t2) a ->
              EffP f2 t1 t2 (a,s2)
runStateP1 _ s (PureP x)                 = ret (x,s)
runStateP1 v s (ImpureP (LP (ModifyP tr)) k)  = runStateP1 v (tr s) (k s)
runStateP1 v s (ImpureP (RP fx) k)  = ImpureP fx (runStateP1 v s . k)

instance TSProj var s (s,t) (EffP (SumP (StateP var) f2))
instance TSProj var s (t,s) (EffP (SumP f1 (StateP var)))

instance MonadMState var s
         (EffP (SumP (StateP var) f2) (s,t) (s,t)) where
  modify var tr = ImpureP (LP (ModifyP tr)) PureP

instance MonadMState var s
         (EffP (SumP f1 (StateP var)) (t,s) (t,s)) where
  modify var tr = ImpureP (RP (ModifyP tr)) PureP

instance MonadMPState var s1 s2 (s1,t) (s2,t)
         (EffP (SumP (StateP var) f2)) where
  modifyP var tr = ImpureP (LP (ModifyP tr)) PureP

instance MonadMPState var s1 s2 (t,s1) (t,s2)
         (EffP (SumP f1 (StateP var))) where
  modifyP var tr = ImpureP (RP (ModifyP tr)) PureP



tsP2r = runStateP Var2 2 $ runStateP1 Var1 1 tsP2
-- (((),4),'c')

-- Error
-- tsP2'r = runStateP Var2 2 $ runStateP1 Var1 1 tsP2'


tsP5 = do x <- getP Var2
          putP Var2 "str"
          r <- tsP4
          y <- getP Var2
          return (r,x,y)

tsP5r = runStateP Var2 () $ runStateP1 Var1 1 tsP5 
-- ((((1,True,'a'),(),"str"),'a'),"str")


runAll :: P.IO ()
runAll =  
  P.print tsP1r  P.>>
  P.print tsP2r  P.>>
  P.print tsP4r  P.>>
  P.print tsP5r
  
