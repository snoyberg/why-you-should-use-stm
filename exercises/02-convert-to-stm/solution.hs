#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent.STM

main :: IO ()
main = do
  var <- atomically $ newTVar "Hello"

  origVal <- atomically $ readTVar var
  atomically $ writeTVar var (origVal ++ " World")

  newVal <- atomically $ readTVar var
  putStrLn newVal

  -- OR
  var <- newTVarIO "Hello"

  -- OR
  atomically $ do
    origVal <- readTVar var
    writeTVar var (origVal ++ " World")

  -- OR
  var <- atomically $ do
    var <- newTVar "Hello"
    modifyTVar var (++ " World")
    pure var
  atomically (readTVar var) >>= putStrLn
