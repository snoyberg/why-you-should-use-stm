#!/usr/bin/env stack
-- stack --resolver lts-13.21 script --compile --ghc-options -rtsopts --ghc-options -threaded
import Control.Concurrent.Async
import Control.Concurrent.STM
import Test.Hspec

-- Fix the sendMoney funciton, changing nothing else!
sendMoney
  :: TVar Int -- ^ from account
  -> TVar Int -- ^ to account
  -> IO ()
sendMoney from to = atomically $ do
  let amount = 5
  fromOrig <- readTVar from
  toOrig <- readTVar to
  writeTVar from $ fromOrig - amount
  writeTVar to $ toOrig + amount

main :: IO ()
main = hspec $ it "no data races" $ do
  from <- newTVarIO 500000
  to <- newTVarIO 0
  replicateConcurrently_ 100000 $ sendMoney from to
  fromFinal <- atomically $ readTVar from
  fromFinal `shouldBe` 0
  toFinal <- atomically $ readTVar to
  toFinal `shouldBe` 500000
