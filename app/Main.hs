{-# LANGUAGE OverloadedStrings#-}
--{-# LANGUAGE RecordWildCards #-}

module Main where

import TelegramConfig
import TelegramJson
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
import Control.Concurrent (threadDelay)
import Lib
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Control.Monad.State
import Control.Exception

data MessageInfo = MessageInfo {mess :: TelegramMessage,
                                wasRepeat :: Bool}

main :: IO ()
main = do
  putStrLn "telegram bot is running!"
  strConf <- B.readFile "app/config/config.txt"
  let conf = eitherDecode strConf :: Either String TelegramConfig
  either putStrLn startBot conf

startBot :: TelegramConfig -> IO ()
startBot c@TelegramConfig{token = t} = do
  let botUrl = "https://api.telegram.org/bot" ++ t ++ "/"
      getUpdates = botUrl ++ "getUpdates"
  evalStateT (checkUpdates c getUpdates botUrl) 0 --maybe I should save id from previous session somewhere

checkUpdates :: TelegramConfig -> String -> String -> StateT Integer IO ()
checkUpdates c getUpdates botUrl = do
  oldId <- get
  updatesStr <- liftIO $ catch (simpleHttp getUpdates) $ return . handleHttpException
  let updates = case updatesStr of
        "" -> Left "Didn't get an answer for request, but I'm still working!"
        upd -> eitherDecode upd :: Either String Updates
  maybe (checkUpdates c getUpdates botUrl) (\mInfo -> do
    let msg = mess mInfo
    if wasRepeat mInfo
       then checkUpdates c{repeats = getRepeats msg} getUpdates botUrl
       else do
         newId <- liftIO $ sendMessage c botUrl msg --maybe I should build request strings only once, somewhere above
         put newId
         checkUpdates c getUpdates botUrl) (processUpdates c botUrl oldId updates)


handleHttpException :: SomeException -> B.ByteString --add normal exception handling
handleHttpException e = ""

getRepeats :: TelegramMessage -> Int
getRepeats = read . text

processUpdates :: TelegramConfig -> String -> Integer -> Either String Updates -> Maybe MessageInfo
processUpdates c botUrl lastId = either (const Nothing) (findLastMessage lastId . result) --case result updates of

findLastMessage :: Integer -> [Update] -> Maybe MessageInfo --simplified version!!
findLastMessage oldId [] = Nothing
findLastMessage oldId (x:[]) = let mess = message x
                                   messId = message_id mess
                                   in if messId > oldId then Just $ MessageInfo mess False
                                                        else Nothing
findLastMessage oldId (x:y:[]) = let mess = message y
                                     messId = message_id mess
                                     in if messId > oldId then
                                       if (text $ message x) == "/repeat"
                                          then Just $ MessageInfo mess True
                                          else Just $ MessageInfo mess False
                                        else Nothing
findLastMessage oldId (x:xs) = findLastMessage oldId xs
{-findLastMessage oldId (x:xs) = --uncomment this to start searching through all messages
  let mess = message x
      messId = message_id mess
      in if messId > oldId then Just mess
                           else findLastMessage oldId xs-}

keyboard :: String
keyboard = "\",\"reply_markup\": {\"keyboard\":[[\"1\",\"2\",\"3\",\"4\",\"5\"]],\"resize_keyboard\": true, \"one_time_keyboard\": true}}"

sendMessage :: TelegramConfig -> String -> TelegramMessage -> IO Integer
sendMessage c@TelegramConfig{repeats = r} botUrl mess =
  case text mess of
    "/help" -> sendInfo c botUrl mess
    "/repeat" -> sendInfo c botUrl mess
    txt -> do
       let sendTxt = send (botUrl ++ "sendMessage")
             (RequestBodyBS $ pack $ "{\"chat_id\": "++ show (chat_id $ chat mess) ++
             ",\"text\": \"" ++ txt ++ "\"}")
       replicateM_ r sendTxt
       return $ message_id mess

sendInfo :: TelegramConfig -> String -> TelegramMessage -> IO Integer
sendInfo TelegramConfig{help = h, repeats = r} botUrl mess = do
  let txt = text mess
      textToSend = case txt of
        "/help" -> h ++ "\"}"
        "/repeat" -> "select repeats count:" ++ keyboard
  send (botUrl ++ "sendMessage") (RequestBodyBS $ pack $ "{\"chat_id\": "++ show (chat_id $ chat mess) ++
   ",\"text\": \"" ++ textToSend)
  return $ message_id mess

handleError :: Integer -> String -> Integer
handleError mId _ = mId --maybe I should add error processing