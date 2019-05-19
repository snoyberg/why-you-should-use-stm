#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Test.Hspec

newtype TMVar a = TMVar (TVar (Maybe a))

-- Make it compile, and make the tests pass

newTMVar :: a -> STM (TMVar a)
newTMVar a = TMVar <$> newTVar (Just a)

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar var) = do
  x <- readTVar var
  case x of
    Nothing -> error "What should we do if there's nothing here?"
    Just a -> do
      writeTVar var Nothing
      error "do something with this:" a

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar var) = do
  x <- readTVar var
  -- Make sure the var is empty after this, you'll need to add something here
  pure x

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar var) a = do
  x <- readTVar var
  case x of
    Nothing -> writeTVar var $ a -- yup, this won't compile, the $ is a hint
    Just _ -> error "What should we do if there's soemthing here?"

-- | Returns True if put was successful, False otherwise
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar var) a = do
  x <- readTVar var
  case x of
    Nothing -> do
      writeTVar var (Just a)
      pure undefined
    Just _ -> pure undefined

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
