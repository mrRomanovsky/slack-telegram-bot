{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestBody = body })

send :: String -> RequestBody -> IO ()
send url s = do
  manager <- newManager tlsManagerSettings --maybe I should make one global manager
  request <- buildRequest url s
  response <- httpLbs request manager
  return ()
  --let Just obj = decode (responseBody response)
  --print (obj :: Object)