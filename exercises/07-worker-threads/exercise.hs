#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (finally)
import Data.Foldable (for_)
import Test.Hspec

-- | Keeps performing work items from the queue
worker :: (a -> IO ()) -> TBMQueue a -> IO ()
worker f queue = loop
  where
    loop = do
      -- Quite a few things are wrong here
      a <- readTBMQueue queue
      f a
      loop

workers :: Int -> (a -> IO ()) -> TBMQueue a -> IO ()
workers count f queue = replicateConcurrently_ count (worker f queue)

pooledMapConcurrently_ :: Int -> (a -> IO ()) -> [a] -> IO ()
pooledMapConcurrently_ count f inputs = do
  queue <- atomically $ newTBMQueue $ count * 2
  let filler = pure () -- I don't think this is what you want
  concurrently_
    (filler `finally` atomically (closeTBMQueue queue))
    (workers count f queue)

main :: IO ()
main = hspec $ it "works" $ do
  var <- newTVarIO (0 :: Int)
  let inputs = [1..1000]
      f input = atomically $ modifyTVar' var (+ input)
  pooledMapConcurrently_ 8 f inputs
  atomically (readTVar var) `shouldReturn` sum inputs
