{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
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

send :: String -> RequestBody -> IO ()
send url s = do
  let logManager =
        tlsManagerSettings {managerModifyRequest = \r -> print r >> return r}
  manager <- newManager logManager
  request <- buildRequest url s
  response <- httpLbs request manager
  return ()

buildRequestSlack :: String -> RequestBody -> IO Request
buildRequestSlack url body = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")]
       })

sendSlack :: String -> RequestBody -> IO ()
sendSlack url s = do
  let logManager =
        tlsManagerSettings {managerModifyRequest = \r -> print r >> return r}
  manager <- newManager logManager
  request <- buildRequestSlack url s
  response <- httpLbs request manager
  return ()       
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