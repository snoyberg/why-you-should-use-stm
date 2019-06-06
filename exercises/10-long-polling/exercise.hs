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
  let status = "Initial status" -- needs to be mutable somehow

  run 3000 $ \req send ->
    case pathInfo req of
      -- Serve the static files
      [] -> send $ responseFile status200 [("content-type", "text/html; charset=utf-8")] "index.html" Nothing
      ["script.js"] -> send $ responseFile status200 [("content-type", "application/javascript; charset=utf-8")] "script.js" Nothing

      -- Modify the status
      ["set-status"] -> do
        (params, _) <- parseRequestBody lbsBackEnd req
        newStatus <-
          case lookup "status" params of
            Nothing -> error "Invalid request"
            Just newStatus -> pure newStatus
        putStrLn "It sure would be nice if we used the new status somehow..."
        send $ responseBuilder status303 [("location", "/")] ""

      -- Get the current status
      ["get-status"] -> do
        oldStatus <-
          case lookup "status" $ queryString req of
            Just (Just oldStatus) -> pure oldStatus
            _ -> error "Invalid request"

        -- We'd like some mutability and blocking please
        let newStatus = status

        send $ responseBuilder status200 [] $ byteString newStatus
