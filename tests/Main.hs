{-# LANGUAGE ExistentialQuantification #-}

-- | 

module Main where

import ThreadLocalLock

import Control.Monad.ST
import Data.STRef
import Control.Concurrent.Async as A
import Control.Exception
import Control.Monad (void)
import Data.List (isInfixOf)

import Test.Tasty
import Test.Tasty.HUnit
            
-- Example
--------------------------------------------------------------------------------

data Example = forall s . Example (TLState s) (STRef s Int)

example :: IO ()
example =
  do e <- newThreadLocalState $ \tls -> do
           r <- newSTRef 3
           return $ Example tls r
     case e of
       Example t r -> withThreadLocalState t (incrSTRef r)
     x <- case e of
           Example t r -> withThreadLocalState t (readSTRef r)
     assertEqual "" 4 x

-- | This will throw an exception.
example2 :: IO ()
example2 =
  do e <- newThreadLocalState $ \tls -> do
           r <- newSTRef 3
           return $ Example tls r
     case e of
       Example t r -> withThreadLocalState t (incrSTRef r)
     a <- async $ case e of
                   Example t r -> withThreadLocalState t (readSTRef r)
     assertExn "but used on thread" $ void (wait a)
                                                      
incrSTRef :: STRef s Int -> ST s ()
incrSTRef r =
    do n <- readSTRef r
       writeSTRef r (n+1)

assertExn :: String -> IO () -> IO ()
assertExn msg act =
  catch act (\e ->
   if isInfixOf msg (show (e :: SomeException))
   then return ()
   else error $ "Expected exception containing message "++ show msg++
                ", instead got this:\n "++ show e)

main :: IO ()
main = defaultMain $ testGroup "tests" $
       [ testCase "example" example
       , testCase "example2" example2
       ] 
