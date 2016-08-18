{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}


module THFresh where

import Language.Haskell.TH
import Data.Proxy

-- do $x         <- newIso
--    $(fresh y) <- newIso

-- Pattern (_:: Var "x", x )
--  newIso :: MonadState v s m =>  m (Proxy (Var v), IsoRef Double)
 
x :: Q Pat
-- x = return $ LitP (TupP ())
-- x = return $ LitP (CharL 'a')
x = do nm <- newName "x"
       return (x' nm)

x' nm = TupP [ proxPat $ ConT ''Int
             -- , VarP 'x -- "Qualified name in binding position"
             , VarP nm
             ]

proxPat :: Type -> Pat
proxPat ty = SigP (ConP 'Proxy []) (AppT (ConT ''Proxy) ty)

pattern Blah = (Proxy::Proxy Int)

pattern Goo = ("hi",333)

y = case ("hi",33) of Goo -> 99

z = case (Proxy::Proxy Int) of Blah -> 99
-- z = case (Proxy::Proxy Double) of Blah -> 99

