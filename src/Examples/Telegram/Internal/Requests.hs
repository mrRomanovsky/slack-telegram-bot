{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.Requests
  ( sendTelegram
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Logging.Config
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders = [(HTTP.hContentType, "application/json")]
       })

sendTelegram :: LogConfig -> String -> RequestBody -> IO ()
sendTelegram LogConfig {logLevel = l, logFile = f} url s = do
  let logManager =
        if l == Debug
          then tlsManagerSettings
                 { managerModifyRequest =
                     \r ->
                       appendFile f "request to server: " >>
                       appendFile f (show r) >>
                       return r
                 , managerModifyResponse =
                     \r ->
                       appendFile f "response status: " >>
                       appendFile f (show $ responseStatus r) >>
                       return r
                 }
          else tlsManagerSettings
  manager <- newManager logManager
  request <- buildRequest url s
  response <- httpLbs request manager
  return ()
