-- | A small test of the sketches in this directory.

{-# Language ScopedTypeVariables, KindSignatures #-}
{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}

{-# Language NoMonomorphismRestriction #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
-- {-# Language GADTs, FunctionalDependencies #-}

module Main where

import ParamEff1
import THFresh
import Data.Proxy
import Data.IORef
import GHC.TypeLits
  
-- Experimenting with TH and pattern synonyms.
--------------------------------------------------------------------------------

readRef :: Ref a -> IO a
readRef (Ref r) = readIORef r

-- | Return a reference which is identified by a type level string.
newRef :: forall (s :: Symbol) a .
         a -> IO (Proxy s, Ref a)
newRef v =
  do r <- newIORef v
     return (Proxy, Ref r)

-- | A "reference" that is just an index into the state.
data RefInd (a::Symbol) = RefInd
            
new :: forall (s :: Symbol) a m . Monad m => 
       m (Proxy s, RefInd s)
new =
  do return (Proxy, RefInd)

new' :: forall (s :: Symbol) a . (Proxy s, RefInd s)
new' = (Proxy, RefInd)

proxSym :: forall (s :: Symbol) . Proxy s
proxSym = Proxy
            
            
--------------------------------------------------------------------------------

{- Changing the type of this Proxy yields this error message:

    Couldn't match kind ‘*’ with ‘Symbol’
    Expected type: Proxy
                     "x_(422,8)_/Users/rrnewton/working_copies/msr-visit/isoref-sketch/IndexedMonad/ParamEff1.hs"
      Actual type: Proxy Int
-}
a = case (Proxy, "hi") of 
       $x -> x

b = do $x <- newRef "contents"
       v  <- readRef x
       return $ "yay: "++v

prox1 :: Proxy "hi"
prox1 = Proxy

-- c :: (Monad (m u u), MonadMPState (Proxy "hi") b Char s u m,
--       TSProj (Proxy "hi") b s m, TSProj (Proxy "hi") Char u m) =>
--      m s u ()
c = putP prox1 'c' 

cr :: ((), Char)
cr = runStateP prox1 'a' c

-- | Here's an example where we use the TH-generated type-level index
-- together with the MonadMPState infrastructure from ParamEff1.hs
d :: IO ()
d = do pat @ $x <- newRef "ignored"
       print =<< readRef x
       print $ runStateP (fst pat) "nil" $
               putP (fst pat) "payload"

-- e = do x <- $freshRef "contents"
--        v  <- readRef x
--        return $ "yay: "++v

f = do $(fresh "quux") <- newRef "fval"b
       v  <- readRef quux
       return $ "yay: "++v

{-
g = do -- $x <- new
       let $x = new' 
       putP x "payload"
       x' <- getP x
       return $ "yay: "++x'
-}

-- Needs RebindableSyntax:
g = let $x = new' in    
    putP x "payload" >>>= \_ ->
    getP x           >>>= \x' ->
    ret $ "yay: "++x'

-- We can't actually run g.        
-- And... running with the wrong state results in a cryptic error: 
-- gr :: (String, String)
-- gr = runStateP Var1 () g 

-- | We can do this instead, but this doesn't really give us the
-- lexical scope that we want.
h = let $x = new' in
    runStateP x "unused" $ 
      putP x "payload" >>>= \_ ->
      getP x           >>>= \x' ->
      ret ()

myRun fn = runStateP r (error "unusued initial state") (fn (Proxy,r))
  where r = RefInd

myRun2 :: forall (s :: Symbol) s1 s2 a .
          ((Proxy s) -> EffP (StateP (Proxy s)) s1 s2 a)
       -> (a, s2)
myRun2 fn = runStateP p (error "unusued initial state") (fn (p,p))
  where p = Proxy

-- i = myRun2 $ \ $x -> getP x


            
data Var1 = Var1

-- No instance for (Monad (m0 u0 u0)) arising from a use of ‘putP’
--     The type variables ‘m0’, ‘u0’ are ambiguous
-- tsP1 = putP Var1 'c'  >>>= \_ ->
--        putP Var1 True

main = do
   print a
   print =<< b
   print cr
   d
   print =<< f

data Ref2 p = Ref2 


           
