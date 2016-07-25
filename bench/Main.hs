{-# LANGUAGE ExistentialQuantification #-}

-- | 

module Main where

import Control.Concurrent
import Control.Exception
import Criterion.Main
import Criterion.Types
import Data.IORef
import Data.STRef
import MVarLock
import ThreadLocalLock
import Control.Monad.ST

import Data.Int


pureCounter :: Int64 -> IO ()
pureCounter iters =
    do let loop cnt
            | cnt == iters = cnt
            | otherwise = loop (cnt+1)
       _ <- evaluate (loop 0)
       return ()
    
pureCounterIO :: Int64 -> IO ()
pureCounterIO iters =
    do let loop cnt
            | cnt == iters = return cnt
            | otherwise = loop (cnt+1)
       _ <- loop 0
       return ()
    
iorefCounter :: Int64 -> IO ()
iorefCounter iters =
    do r <- newIORef 0
       let loop cnt
            | cnt == iters = readIORef r
            | otherwise = do writeIORef r cnt 
                             loop (cnt+1)
       _ <- loop 0
       return ()
              
-- | UNSAFE direct use of ST.  
strefCounter :: Int64 -> IO ()
strefCounter iters = stToIO $ 
    do r <- newSTRef 0
       let loop cnt
            | cnt == iters = readSTRef r
            | otherwise = do writeSTRef r cnt 
                             loop (cnt+1)
       _ <- loop 0
       return ()

data Example = forall s . Example (TLState s) (STRef s Int64)

-- | Amortize all accesses with a single check.  This is the best case.
tlsCounterAmortized :: Int64 -> IO ()
tlsCounterAmortized iters =
  do e <- newThreadLocalState $ \tls -> do
           r <- newSTRef 3
           return $ Example tls r
     _ <- case e of
           Example t r -> withThreadLocalState t $
             let loop cnt
                  | cnt == iters = readSTRef r
                  | otherwise = do writeSTRef r cnt 
                                   loop (cnt+1)
             in loop 0
     return ()

-- | Models many small, fine grained accesses to the thread-locked state.
--   This is the worst case.
tlsCounter :: Int64 -> IO ()
tlsCounter iters =
  do e <- newThreadLocalState $ \tls -> do
           r <- newSTRef 3
           return $ Example tls r
     let loop cnt
           | cnt == iters =
               case e of
                Example t r -> withThreadLocalState t $ readSTRef r
           | otherwise = do
               case e of
                 Example t r -> withThreadLocalState t $ writeSTRef r cnt 
               loop (cnt+1)
     _ <- loop 0
     return ()

-- | In this version, only do the pattern match once and keep the
-- lock's key in scope
tlsCounterOpt :: Int64 -> IO ()
tlsCounterOpt iters =
  do e <- newThreadLocalState $ \tls -> do
           r <- newSTRef 3
           return $ Example tls r
     case e of
      Example tls r -> do
       let loop cnt
             | cnt == iters = withThreadLocalState tls $ readSTRef r
             | otherwise = do withThreadLocalState tls $ writeSTRef r cnt 
                              loop (cnt+1)
       _ <- loop 0
       return ()
            
-- | An STRef protected by a lock.
data Example2 = forall s . Example2 (MVarLock s ()) (STRef s Int64)

-- | Same as tlsCounter, but grabbing a lock at runtime.
mvarLockCounter :: Int64 -> IO ()
mvarLockCounter iters =
  do e <- newMVarLock () $ \mv -> do
           r <- newSTRef 3
           return $ Example2 mv r
     let loop cnt
           | cnt == iters =
               case e of
                Example2 m r -> withMVarLock_ m $ \() -> readSTRef r
           | otherwise = do
               case e of
                 Example2 m r -> withMVarLock_ m $ \() -> writeSTRef r cnt 
               loop (cnt+1)
     _ <- loop 0
     return ()

    
main :: IO ()
main = defaultMain 
       [
         bench "myThreadId"      $ whnfIO myThreadId
       , bench "PureCounter"     $ Benchmarkable pureCounter
       , bench "PureCounterIO"   $ Benchmarkable pureCounterIO
       , bench "mvarLockCounter" $ Benchmarkable mvarLockCounter
       , bench "tlsCounter"      $ Benchmarkable tlsCounter
       , bench "tlsCounterOpt"   $ Benchmarkable tlsCounterOpt
       , bench "tlsCounterAmortized" $ Benchmarkable tlsCounterAmortized
       , bench "IORef" $ Benchmarkable iorefCounter
       , bench "STRef" $ Benchmarkable strefCounter
       ]
