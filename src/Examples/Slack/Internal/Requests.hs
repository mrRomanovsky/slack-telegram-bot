{-# LANGUAGE OverloadedStrings #-}

module Examples.Slack.Internal.Requests
  ( sendSlack
  , postMessageSlack
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Examples.Requests.RequestBuilders
import Logging.Config
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP

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

postMessageSlack :: LogConfig -> String -> [RequestParam] -> String -> IO ()
postMessageSlack lc method params botToken = do
  let url = slackUrl ++ method
      body = buildRequestBody params
  request <- buildPostRequestSlack url body botToken
  sendRequestSlack lc request
  return ()

sendSlack :: LogConfig -> String -> [RequestParam] -> IO ByteString
sendSlack lc method params = do
  let url = slackUrl ++ method
      query = buildQueryParams params
  request <- buildRequestSlack url query
  sendRequestSlack lc request

sendRequestSlack :: LogConfig -> Request -> IO ByteString
sendRequestSlack LogConfig {logLevel = logL, logFile = logF} request = do
  let logManager =
        if logL == Debug
          then tlsManagerSettings
                 { managerModifyRequest =
                     \r ->
                       appendFile logF "request to server: " >>
                       appendFile logF (show r) >>
                       return r
                 , managerModifyResponse =
                     \r ->
                       appendFile logF "response status: " >>
                       appendFile logF (show $ responseStatus r) >>
                       return r
                 }
          else tlsManagerSettings
  manager <- newManager logManager
  response <- httpLbs request manager
  return $ responseBody response

slackUrl = "https://slack.com/api/"
