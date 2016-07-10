{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- | 

module MVarLock where

import Control.Concurrent.MVar
-- import GHC.Conc

import Control.Monad.ST
import Data.STRef

--------------------------------------------------------------------------------

data MVarLock s a = MVar a


withMVarLock :: MVarLock s a
             -> (a -> ST s (a, b))
             -> IO b
withMVarLock = undefined


-- FINISHME:
-- Need to make make this a higher rank type and then "teleport" the
-- 'a' result out of it.  Don't let the b result have any mutable bits.
newMVarLock :: ST s (a,b) -> IO (MVarLock s a, b)
newMVarLock = undefined

--------------------------------------------------------------------------------

class STCastable (a :: *) where
  -- What is the 's' param of this type:
  type GetState a :: *
  -- What
  type WithState s a :: *


-- Example datatype:
--------------------------------------------------------------------------------

data MyRec = forall s . MyRec (MVarLock s Bool) (STRef s Int)

mkMyRec :: IO MyRec
mkMyRec =
  do (mv,r) <- newMVarLock (do r <- newSTRef 3
                               return (True, r))
     return $ MyRec mv r



modRec :: MyRec -> IO ()
modRec (MyRec mv r) =
  withMVarLock mv $ \ b -> do
    n <- readSTRef r
    writeSTRef r $! (n+1)
    return (not b, ())

