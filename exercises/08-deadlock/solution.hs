#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent.STM

main :: IO ()
main = do
  var <- newTVarIO 5
  atomically $ do
    x <- readTVar var
    check (x >= 10)
    writeTVar var $ x - 10
