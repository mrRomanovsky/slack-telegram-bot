{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.Requests
  ( sendTelegram
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
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

sendTelegram :: String -> RequestBody -> IO ()
sendTelegram url s = do
  let logManager =
        tlsManagerSettings
          { managerModifyRequest =
              \r -> writeFile "telegram.log" (show r) >> return r
          }
  manager <- newManager logManager
  request <- buildRequest url s
  response <- httpLbs request manager
  return ()
