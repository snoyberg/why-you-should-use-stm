#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Time

myThreadDelay :: Int -> IO ()
myThreadDelay micros = do
  putStrLn "This isn't right..."

myTimeout :: Int -> IO a -> IO (Maybe a)
myTimeout micros action = do
  putStrLn "Neither is this"
  Just <$> action

main :: IO ()
main = do
  putStrLn "myThreadDelay"
  getCurrentTime >>= print
  myThreadDelay 1000000
  getCurrentTime >>= print

  putStrLn "\nmyTimeout"
  getCurrentTime >>= print
  myTimeout 1000000 (myThreadDelay 5000000) >>= print
  getCurrentTime >>= print
