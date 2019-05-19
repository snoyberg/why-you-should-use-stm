#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent.STM
import System.IO.Unsafe -- yeah baby!

counter :: TVar Int
counter = unsafePerformIO $ newTVarIO 0
{-# NOINLINE counter #-} -- very important!

main :: IO ()
main = do
  atomically $ modifyTVar' counter (+ 1)
  atomically $ modifyTVar' counter (+ 1)
  atomically $ modifyTVar' counter (+ 1)
  count <- atomically $ readTVar counter
  putStrLn $ "The count is: " ++ show count
