#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent.STM

main :: IO ()
main = do
  -- You'll need to change some function names, and add in some calls to
  -- atomically

  var <- newTVar "Hello"

  origVal <- readTVar var
  writeIORef var (origVal ++ " World")

  newVal <- readIORef var
  putStrLn newVal
