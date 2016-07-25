{-# LANGUAGE RankNTypes #-}

-- | State that is locked to one thread. 

module ThreadLocalLock
    (TLState, newThreadLocalState, withThreadLocalState )
    where

import Control.Concurrent
import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.STRef
      

-- | A lock around thread local state.
newtype TLState s = TLState ThreadId

-- | Use this to construct an existential package that closes over the
-- `s` parameter.  The package returned is bound to the *current thread*.
newThreadLocalState :: (forall s . TLState s -> ST s b) -> IO b
newThreadLocalState fn = do
  tid <- myThreadId 
  return $! runST (fn (TLState tid))
{-# INLINABLE newThreadLocalState #-}

withThreadLocalState :: TLState s -> ST s b -> IO b
withThreadLocalState (TLState t1) s =
    do t2 <- myThreadId
       if t1 == t2
        then unsafeSTToIO s
        else error $ "withThreadLocalState: state bound to thread " ++ 
                     show t1 ++ " but used on thread "++ show t2
{-# INLINABLE withThreadLocalState #-}
