{-# LANGUAGE OverloadedStrings#-}
--{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import TelegramJson
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
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
  checkUpdates c getUpdates botUrl
  --liftIO $ threadDelay 1000 --remove this later!

handleHttpException :: SomeException -> B.ByteString --add normal exception handling
handleHttpException e = ""

processUpdates :: Config -> String -> Integer -> Updates -> IO Integer
processUpdates c botUrl lastId updates = case result updates of
  [] -> return lastId
  rs -> do
    let mess = findLastMessage lastId rs
    maybe (return lastId) (sendMessage c botUrl) mess

findLastMessage :: Integer -> [Update] -> Maybe Message
findLastMessage oldId [] = Nothing
findLastMessage oldId (x:xs) =
  let mess = message x
      messId = message_id mess
      in if messId > oldId then Just mess
                           else findLastMessage oldId xs

keyboard :: String
keyboard = "&reply_markup={\"keyboard\":[[\"Yes\",\"No\"],[\"Maybe\"],[\"1\",\"2\",\"3\"]]}"

sendMessage :: Config -> String -> Message -> IO Integer
sendMessage Config{help = h, repeats = r} botUrl mess = do
  let txt = text mess
      textToSend = case txt of
        "/help" -> h
        "/repeat" -> "test" ++ keyboard
        _        -> txt
  let sendUrl = botUrl ++ "sendMessage?chat_id="
             ++ show (chat_id $ chat mess) ++ "&text=" ++ textToSend
  putStrLn sendUrl
  simpleHttp sendUrl
  return $ message_id mess

handleError :: Integer -> String -> IO Integer
handleError id err = do
  putStrLn err
  return id