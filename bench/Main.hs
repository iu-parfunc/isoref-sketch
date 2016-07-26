{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}

-- | 

module Main where

import qualified Data.Atomics.Counter as C
import Control.Concurrent
import Control.Exception
import Criterion.Main
import Criterion.Types
import Data.IORef
import qualified GHC.IORef as GIO
import Data.STRef
import MVarLock
import ThreadLocalLock
import Control.Monad.ST

import Data.Int

three :: Int64
three = 3

-- Replace all occurences of "three" with to write increasing,
-- re-allocated, re-boxed Ints.

#ifdef CONSTANT
#warning "Using a CONSTANT argument to writeIORef"
#define _WHATTOWRITE three
#else
#warning "Using a INCREASING (reboxed) argument to writeIORef"
#define _WHATTOWRITE cnt
#endif
        
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
            | otherwise = do writeIORef r _WHATTOWRITE
                             loop (cnt+1)
       _ <- loop 0
       return ()

unsafeIorefCounter :: Int64 -> IO ()
unsafeIorefCounter iters =
    do r <- GIO.newIORef 0
       let loop cnt
            | cnt == iters = GIO.readIORef r
            | otherwise = do GIO.writeIORef r _WHATTOWRITE
                             loop (cnt+1)
       _ <- loop 0
       return ()
              
atomicIORefCounter :: Int64 -> IO ()
atomicIORefCounter iters =
    do r <- newIORef 0
       let loop cnt
            | cnt == iters = readIORef r
            | otherwise = do atomicWriteIORef r _WHATTOWRITE
                             loop (cnt+1)
       _ <- loop 0
       return ()

-- | This is not really a fair comparison because it's really doing a
-- read and a write (RMW).
atomicCounter :: Int64 -> IO ()
atomicCounter iters =
    do r <- C.newCounter 0
       let loop cnt
            | cnt == iters = C.readCounter r
            | otherwise = do C.incrCounter_ 1 r 
                             loop (cnt+1)
       _ <- loop 0
       return ()
              
              
-- | UNSAFE direct use of ST.  
strefCounter :: Int64 -> IO ()
strefCounter iters = stToIO $ 
    do r <- newSTRef 0
       let loop cnt
            | cnt == iters = readSTRef r
            | otherwise = do writeSTRef r _WHATTOWRITE
                             loop (cnt+1)
       _ <- loop 0
       return ()

data Example = forall s . Example (TLState s) (STRef s Int64)

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
                 Example t r -> withThreadLocalState t $ writeSTRef r _WHATTOWRITE
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
       -- The compiler SHOULD be be able to lift the TID check out of the loop
       -- here and optimize this into tlsCounterAmortized.  It does not.
       -- In fact, this "Opt" version doesn't go any faster at all.
       let loop cnt
             | cnt == iters = withThreadLocalState tls $ readSTRef r
             | otherwise = do withThreadLocalState tls $ writeSTRef r _WHATTOWRITE
                              loop (cnt+1)
       _ <- loop 0
       return ()

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
                  | otherwise = do writeSTRef r _WHATTOWRITE
                                   loop (cnt+1)
             in loop 0
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
       , bench "UnsafeIORef" $ Benchmarkable unsafeIorefCounter
       , bench "IORef" $ Benchmarkable iorefCounter
       , bench "AtomicIORef" $ Benchmarkable atomicIORefCounter
       , bench "AtomicCounter" $ Benchmarkable atomicCounter
       , bench "STRef" $ Benchmarkable strefCounter
       ]
