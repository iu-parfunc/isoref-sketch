-- | A small test of the sketches in this directory.

{-# Language ScopedTypeVariables, KindSignatures #-}
{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}

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

main = do
   print a
   print =<< b
   print cr
   d
