#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Test.Hspec

newtype TMVar a = TMVar (TVar (Maybe a))

newTMVar :: a -> STM (TMVar a)
newTMVar a = TMVar <$> newTVar (Just a)

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar var) = do
  x <- readTVar var
  case x of
    Nothing -> retry
    Just a -> do
      writeTVar var Nothing
      pure a

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar var) = do
  x <- readTVar var
  writeTVar var Nothing
  pure x

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar var) a = do
  x <- readTVar var
  case x of
    Nothing -> writeTVar var $ Just a
    Just _ -> retry

-- | Returns True if put was successful, False otherwise
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar var) a = do
  x <- readTVar var
  case x of
    Nothing -> do
      writeTVar var (Just a)
      pure True
    Just _ -> pure False

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
