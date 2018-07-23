{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramBot where

import TelegramConfig
import TelegramJson
import EchoBot
import Control.Monad.State
import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
import Lib
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Control.Exception


data TelegramBot = TelegramBot { config :: TelegramConfig
                               , botUrl :: String
                               , getUpdates :: String
                               , lastMessId :: Integer }

instance EchoBot TelegramBot TelegramMessage TelegramConfig where
  getBotWithConfig c =
    let bUrl = "https://api.telegram.org/bot" ++ token c ++ "/"
        in TelegramBot c bUrl (bUrl ++ "getUpdates") 0

  --getLastMessage :: StateT TelegramBot IO TelegramMessage
  getLastMessage = do
    tBot@TelegramBot{config = c, getUpdates = updStr, lastMessId = oldId} <- get
    updatesStr <- liftIO $ catch (simpleHttp updStr) $ return . handleHttpException
    let updates = case updatesStr of
          "" -> Left "Didn't get an answer for request, but I'm still working!"
          upd -> eitherDecode upd :: Either String Updates
        m = processUpdates c oldId updates
    put $ maybe tBot (\msg -> tBot{lastMessId = message_id msg}) m
    return m

  processMessage b@TelegramBot{config = c, botUrl = bUrl} m = do
    let txt = text m
        textToSend = case txt of --implement option for parsing plain text!
          "/help"   -> help c ++ "\"}"
          "/repeat" -> "select repeats count:" ++ keyboard
          t         -> t ++ "\"}"
    send (bUrl ++ "sendMessage") (RequestBodyBS $ pack $ "{\"chat_id\": "++ show (chat_id $ chat m) ++
      ",\"text\": \"" ++ textToSend)
    return $ b{lastMessId = message_id m}


keyboard :: String
keyboard = "\",\"reply_markup\": {\"keyboard\":[[\"1\",\"2\",\"3\",\"4\",\"5\"]],\"resize_keyboard\": true, \"one_time_keyboard\": true}}"


processUpdates :: TelegramConfig -> Integer -> Either String Updates -> Maybe TelegramMessage
processUpdates c lastId = either (const Nothing) (findLastMessage lastId . result) --case result updates of

findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage --simplified version!!
findLastMessage oldId [] = Nothing
findLastMessage oldId (x:xs) = let mess = message x
                                   messId = message_id mess
                                   in if messId > oldId
                                         then Just mess
                                         else findLastMessage oldId xs

handleHttpException :: SomeException -> B.ByteString --add normal exception handling
handleHttpException e = ""

{-instance BotMessage TelegramMessage where
  getMessageText = text

instance BotConfig TelegramConfig where
  getRepeatsCount = repeats

  setRepeatsCount rc tc = tc{repeats = rc}

  getHelpMessage = help-}