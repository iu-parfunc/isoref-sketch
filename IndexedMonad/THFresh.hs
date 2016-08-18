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

-- | Premade identifiers, ready to go.
x :: Q Pat
x = fresh "x"

y :: Q Pat
y = fresh "y"

z :: Q Pat
z = fresh "z"

-- | The user can make their own identifiers like so.
fresh :: String -> Q Pat
fresh str =
    do nm <- newName str
       unique <- mkUnique str
       return $ TupP [ proxPat unique, VarP nm]

proxPat :: String -> Pat
proxPat str = SigP (ConP 'Proxy []) (proxTy str)

proxTy :: String -> Type
proxTy str = (AppT (ConT ''Proxy) (LitT (StrTyLit str)))
  
-- FIXME: Pair should probably be represented by an custom datatype to
-- give better error messages.

mkUnique :: String -> Q String
mkUnique str = do 
       Loc{loc_filename,loc_start} <- location
       -- There must be a better way than this:
       -- Maybe a cryptographic hash?  Well, at least this is readable:
       return $ str ++ "_" ++ show loc_start ++"_"++ loc_filename
       -- A custom error presentation could perhaps parse them back out....
       -- Another idea would be to put a wrapper around this and change how they print.

-- This should probably be tagged with its type-level identitifier:
newtype Ref a = Ref (IORef a)


-- FINISHME: provide an alternative that goes on the RHS:
freshRef :: Q Exp
freshRef =
  do uq <- mkUnique "(freshRef)"
     [| \ val -> 
        do -- FINISHME
           r <- newIORef val
           return (Ref r)
      |]

