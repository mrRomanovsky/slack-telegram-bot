{-# LANGUAGE OverloadedStrings#-}
--{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import TelegramJson
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B
import Control.Monad.State
import Control.Exception

main :: IO ()
main = do
  putStrLn "telegram bot is running!"
  strConf <- B.readFile "app/config/config.txt"
  let conf = eitherDecode strConf :: Either String Config
  either putStrLn startBot conf

startBot :: Config -> IO ()
startBot c@Config{token = t} = do
  let botUrl = "https://api.telegram.org/bot" ++ t ++ "/"
      getUpdates = botUrl ++ "getUpdates"
  evalStateT (checkUpdates c getUpdates botUrl) 0 --maybe I should save id from previous session somewhere

checkUpdates :: Config -> String -> String -> StateT Integer IO ()
checkUpdates c getUpdates botUrl = do
  oldId <- get
  updatesStr <- liftIO $ catch (simpleHttp getUpdates) $ return . handleHttpException
  let updates = case updatesStr of
        "" -> Left "Didn't get an answer for request, but I'm still working!"
        upd -> eitherDecode upd :: Either String Updates
  newId <- liftIO $ either (handleError oldId) (processUpdates c botUrl oldId) updates --maybe I should build request strings only once, somewhere above
  put newId
  liftIO $ threadDelay 1000 --remove this later!
  checkUpdates c getUpdates botUrl

handleHttpException :: SomeException -> B.ByteString --add normal exception handling
handleHttpException e = ""

processUpdates :: Config -> String -> Integer -> Updates -> IO Integer
processUpdates c botUrl lastId updates = case result updates of
  [] -> return lastId
  rs -> do    
    let mes = message $ last $ result  updates
        mesId = message_id mes
        chatId = chat_id $ chat mes
        txt = text mes
    when (mesId > lastId) $ sendMessage c botUrl chatId txt
    return $ max mesId lastId


sendMessage :: Config -> String -> Integer -> String -> IO ()
sendMessage Config{help = h, repeats = r} botUrl chatId txt = do
  let textToSend = case txt of
        "/help" -> h
        _        -> txt
  let sendUrl = botUrl ++ "sendMessage?chat_id="
             ++ show chatId ++ "&text=" ++ textToSend
  simpleHttp sendUrl
  return ()

handleError :: Integer -> String -> IO Integer
handleError id err = do
  putStrLn err
  return id