#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Applicative (Alternative (..))
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar hiding (tryTakeTMVar, tryPutTMVar)
import Test.Hspec

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar tmvar = (Just <$> takeTMVar tmvar) <|> pure Nothing

-- | Returns True if put was successful, False otherwise
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar tmvar a = (True <$ putTMVar tmvar a) <|> pure False

main :: IO ()
main = hspec $ do
  it "takeTMVar" $ do
    var <- atomically $ newTMVar 5
    atomically $ do
      x <- takeTMVar var
      putTMVar var $ x + 1
    atomically (takeTMVar var) `shouldReturn` 6
    atomically (tryTakeTMVar var) `shouldReturn` Nothing
  it "tryTakeTMVar" $ do
    var <- atomically newEmptyTMVar
    atomically (tryTakeTMVar var) `shouldReturn` (Nothing :: Maybe Int)
  it "tryPutTMVar" $ do
    var <- atomically newEmptyTMVar
    atomically (tryPutTMVar var 5) `shouldReturn` True
    atomically (tryPutTMVar var 6) `shouldReturn` False
    atomically (tryTakeTMVar var) `shouldReturn` Just 5
    atomically (tryTakeTMVar var) `shouldReturn` Nothing
  it "putTMVar" $ do
    var <- atomically newEmptyTMVar
    atomically (putTMVar var 5)
    atomically (tryPutTMVar var 6) `shouldReturn` False
    atomically (takeTMVar var) `shouldReturn` 5
