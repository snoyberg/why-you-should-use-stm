#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Time

myThreadDelay :: Int -> IO ()
myThreadDelay micros = do
  delayVar <- registerDelay micros
  atomically $ do
    didDelay <- readTVar delayVar
    check didDelay

myTimeout :: Int -> IO a -> IO (Maybe a)
myTimeout micros action =
  withAsync action $ \thread -> do
    delayVar <- registerDelay micros
    atomically $
      (Just <$> waitSTM thread) <|>
      (Nothing <$ (readTVar delayVar >>= check))

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
