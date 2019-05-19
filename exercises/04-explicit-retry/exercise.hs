#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)

-- Tasks:
--
-- 1. Make it compile
--
-- 2. Add a retry to prevent the balance from becoming negative
--
-- 3. Fix a race condition
--
-- Bonus: replace the usage of retry with check

-- | Add 5 to the variable every 100ms
payAlice :: TVar Int -> IO ()
payAlice aliceVar = forever $ do
  aliceOrig <- atomically $ readTVar aliceVar
  atomically $ writeTVar aliceVar $! aliceOrig + 5
  threadDelay 100000 -- 100ms, threadDelay takes nanoseconds

-- | Transfer 40 from Alice to Bob.
transfer
  :: TVar Int -- ^ Alice
  -> TVar Int -- ^ Bob
  -> IO ()
transfer aliceVar bobVar = do
  let amt = 40
  aliceOrig <- readTVar aliceVar
  if aliceOrig >= amt
    then pure ()
    else pure ()

  writeTVar aliceVar $ aliceOrig - amt
  bobOrig <- readTVar bobVar
  writeTVar bobVar $ bobOrig + amt

main :: IO ()
main = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0
  race_ (payAlice aliceVar) (transfer aliceVar bobVar)
  alice <- atomically $ readTVar aliceVar
  bob <- atomically $ readTVar bobVar
  putStrLn $ "Alice has: " ++ show alice
  putStrLn $ "Bob has: " ++ show bob
