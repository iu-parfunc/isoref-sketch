{-# LANGUAGE NamedFieldPuns #-}
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

x :: Q Pat
x = go "x"

y :: Q Pat
y = go "y"

z :: Q Pat
z = go "z"

go :: String -> Q Pat
go str =
    do nm <- newName str
       Loc{loc_filename,loc_start} <- location
       -- There must be a better way than this:
       -- Maybe a cryptographic hash?  Well, at least this is readable:
       let unique = str ++ "_" ++ show loc_start ++"_"++ loc_filename
       -- A custom error presentation could perhaps parse them back out....
       -- Another idea would be to put a wrapper around this and change how they print.
       return $ TupP [ proxPat $ LitT (StrTyLit unique)
                     , VarP nm
                     ]
-- FIXME: Pair should probably be represented by an custom datatype to
-- give better error messages.



proxPat :: Type -> Pat
proxPat ty = SigP (ConP 'Proxy []) (AppT (ConT ''Proxy) ty)

newtype Ref a = Ref (IORef a)
