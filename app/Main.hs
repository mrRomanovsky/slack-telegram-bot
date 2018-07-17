{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import TelegramJson
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  putStrLn "hello world"
  strConf <- B.readFile "app/config/config.txt"
  let conf = eitherDecode strConf :: Either String Config
  either putStrLn startBot conf

startBot :: Config -> IO ()
startBot (Config {token = t, repeats = r}) = do
  let botUrl = "https://api.telegram.org/bot" ++ t ++ "/"
      getUpdates = botUrl ++ "getUpdates"
  when True $ do
    updatesStr <- simpleHttp getUpdates
    let updates = eitherDecode updatesStr :: Either String Updates
    either putStrLn (processUpdates botUrl) updates
    threadDelay 1000

processUpdates :: String -> Updates -> IO ()
processUpdates botUrl updates = do
    let mes = message $ last $ result  updates
        chatId = chat_id $ chat $ mes
        txt = text mes
        sendUrl = botUrl ++ "sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ txt
    simpleHttp sendUrl
    return ()