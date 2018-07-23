{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Types as HTTP
import Data.ByteString.Lazy (ByteString)

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders = [(HTTP.hContentType, "application/json")]
       })

send :: String -> RequestBody -> IO ()
send url s = do
  let logManager =
        tlsManagerSettings {managerModifyRequest = \r -> print r >> return r}
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
       , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")]
       , queryString = pack query
       })

buildPostRequestSlack :: String -> RequestBody -> String -> IO Request
buildPostRequestSlack url body botToken = do
  nakedRequest <-parseRequest url
  return
    (nakedRequest
       {method = "POST"
      , requestBody = body
      , requestHeaders = [ (HTTP.hContentType, "application/json")
                         , (HTTP.hAuthorization, pack $ "Bearer " ++ botToken) ]
      })

postMessageSlack :: String -> RequestBody -> String -> IO ()
postMessageSlack url body botToken = do
  let logManager =
        tlsManagerSettings {managerModifyRequest = \r -> print r >> return r}
  manager <- newManager logManager
  request <- buildPostRequestSlack url body botToken
  response <- httpLbs request manager
  print response
  --return ()

sendSlack :: String -> String -> IO ByteString
sendSlack url s = do
  let logManager =
        tlsManagerSettings {managerModifyRequest = \r -> print r >> return r}
  manager <- newManager logManager
  request <- buildRequestSlack url s
  response <- httpLbs request manager
  return $ responseBody response       
  --let Just obj = decode (responseBody response)
  --print (obj :: Object)

{-
  
  botToken = "TOKEN"
  
  chatId = "CHAT_ID"
  
  sendMain = do
    let botUrl = "https://api.telegram.org/bot" ++ botToken ++ "/sendMessage"
        requestBody =
          RequestBodyBS $
          pack $ "{\"chat_id\": " ++ chatId ++ ",\"text\": \"test\"}"
    send botUrl requestBody
-}