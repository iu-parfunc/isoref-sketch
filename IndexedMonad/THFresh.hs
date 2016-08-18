{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

{-# Language KindSignatures #-}
{-# Language DataKinds  #-}

module THFresh where

import Language.Haskell.TH
import Data.Proxy
import GHC.TypeLits
import Data.IORef


-- FIXME: the type level string must encode the static location of the identifier!
x :: Q Pat
x = do nm <- newName "x"
       return $ TupP [ proxPat $ LitT (StrTyLit "x")
                     , VarP nm
                     ]

-- FIXME: Pair should probably be represented by an custom datatype to
-- give better error messages.

proxPat :: Type -> Pat
proxPat ty = SigP (ConP 'Proxy []) (AppT (ConT ''Proxy) ty)

newtype Ref a = Ref (IORef a)

readRef :: Ref a -> IO a
readRef (Ref r) = readIORef r

-- | Return a reference which is identified by a type level string.
freshRef :: forall (s :: Symbol) a .
         a -> IO (Proxy s, Ref a)
freshRef v =
  do r <- newIORef v
     return (Proxy, Ref r)
