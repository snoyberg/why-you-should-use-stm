#!/usr/bin/env stack
-- stack --resolver lts-13.21 script
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Control.Concurrent.STM
import Data.ByteString.Builder (byteString)
import Control.Concurrent (threadDelay)
import GHC.Conc

main :: IO ()
main = do
  tvar <- newTVarIO "Initial status"

  run 3000 $ \req send ->
    case pathInfo req of
      [] -> send $ responseFile status200 [("content-type", "text/html; charset=utf-8")] "index.html" Nothing
      ["script.js"] -> send $ responseFile status200 [("content-type", "application/javascript; charset=utf-8")] "script.js" Nothing
      ["set-status"] -> do
        (params, _) <- parseRequestBody lbsBackEnd req
        newStatus <-
          case lookup "status" params of
            Nothing -> error "Invalid request"
            Just newStatus -> pure newStatus
        atomically $ writeTVar tvar newStatus
        send $ responseBuilder status303 [("location", "/")] ""
      ["get-status"] -> do
        oldStatus <-
          case lookup "status" $ queryString req of
            Just (Just oldStatus) -> pure oldStatus
            _ -> error "Invalid request"
        newStatus <- atomically $ do
          newStatus <- readTVar tvar
          check $ oldStatus /= newStatus
          pure newStatus
        send $ responseBuilder status200 [] $ byteString newStatus
