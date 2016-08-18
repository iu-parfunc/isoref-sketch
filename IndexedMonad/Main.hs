-- | A small test of the sketches in this directory.

{-# Language ScopedTypeVariables, KindSignatures #-}
{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}

module Main where

import ParamEff1
import THFresh
import Data.Proxy
    
-- Experimenting with TH and pattern synonyms.
--------------------------------------------------------------------------------

{- Changing the type of this Proxy yields this error message:

    Couldn't match kind ‘*’ with ‘Symbol’
    Expected type: Proxy
                     "x_(422,8)_/Users/rrnewton/working_copies/msr-visit/isoref-sketch/IndexedMonad/ParamEff1.hs"
      Actual type: Proxy Int
-}
a = case (Proxy, "hi") of 
       $x -> x

b = do $x <- freshRef "contents"
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
-- together with the MonadMPState infrastructure above.
d :: IO ()
d = do pat @ $x <- freshRef "ignored"
       print =<< readRef x
       print $ runStateP (fst pat) "nil" $
               putP (fst pat) "payload"

main = do
   print a
   print =<< b
   print cr
   d
