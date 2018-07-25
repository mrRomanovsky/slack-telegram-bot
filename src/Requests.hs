{-# LANGUAGE OverloadedStrings #-}

module Requests
  ( sendTelegram
  , sendSlack
  , postMessageSlack
  ) where

import Data.Aeson
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

buildRequestSlack :: String -> String -> IO Request
buildRequestSlack url query = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "GET"
       , requestHeaders =
           [(HTTP.hContentType, "application/x-www-form-urlencoded")]
       , queryString = pack query
       })

buildPostRequestSlack :: String -> RequestBody -> String -> IO Request
buildPostRequestSlack url body botToken = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders =
           [ (HTTP.hContentType, "application/json")
           , (HTTP.hAuthorization, pack $ "Bearer " ++ botToken)
           ]
       })

postMessageSlack :: String -> RequestBody -> String -> IO ()
postMessageSlack url body botToken = do
  request <- buildPostRequestSlack url body botToken
  sendRequestSlack request
  return ()

sendSlack :: String -> String -> IO ByteString
sendSlack url s = do
  request <- buildRequestSlack url s
  sendRequestSlack request

sendRequestSlack :: Request -> IO ByteString
sendRequestSlack request = do
  let logManager =
        tlsManagerSettings
          { managerModifyRequest =
              \r -> writeFile "slack.log" (show r) >> return r
          }
  manager <- newManager logManager
  response <- httpLbs request manager
  return $ responseBody response
