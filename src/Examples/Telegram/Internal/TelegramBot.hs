{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.TelegramBot where

import Bot.Bot
import Control.Exception
import Control.Monad (void)
import Control.Monad.State
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Echo.EchoBot
import Examples.Telegram.Internal.Exceptions
import Examples.Telegram.Internal.Requests
import Examples.Telegram.Internal.TelegramConfig
import Examples.Telegram.Internal.TelegramJson
import Network.HTTP.Conduit hiding (httpLbs)

data TelegramBot = TelegramBot
  { config :: TelegramConfig
  , botUrl :: String
  , getUpdates :: String
  , sendMessage :: String
  , lastMessId :: Integer
  , waitingForRepeats :: Bool
  }

instance Bot TelegramBot where
  type BotConfig TelegramBot = TelegramConfig
  type BotMessage TelegramBot = TelegramMessage
  getBotWithConfig c =
    let bUrl = "https://api.telegram.org/bot" ++ token c ++ "/"
     in TelegramBot
          c
          bUrl
          (bUrl ++ "getUpdates")
          (bUrl ++ "sendMessage")
          0
          False
  getLastMessage = do
    tBot@TelegramBot {config = c, getUpdates = updStr, lastMessId = oldId} <-
      get
    updatesStr <-
      liftIO $ try $ simpleHttp updStr :: StateT TelegramBot IO (Either SomeException B.ByteString)
    case updatesStr of
      (Left e) -> do
        liftIO $
          writeFile "telegram.log" $
          "Caught exception while getting last message: " ++ show e
        return Nothing
      (Right upd) -> do
        let updates = eitherDecode upd :: Either String Updates
        let msg = processUpdates oldId updates
        put $ maybe tBot (\m -> tBot {lastMessId = message_id m}) msg
        return msg
  sendMessageTo mChat txt = do
    b@TelegramBot {config = c, sendMessage = sendUrl, waitingForRepeats = wr} <-
      get
    liftIO $ sendText txt (chat_id mChat) sendUrl

sendText :: String -> Integer -> String -> IO ()
sendText txt chatId sendUrl =
  catch
    (sendTelegram
       sendUrl
       (RequestBodyBS $
        pack $ "{\"chat_id\": " ++ show chatId ++ ",\"text\": \"" ++ txt))
    handleException

instance EchoBot TelegramBot where
  helpMessage = help . config
  repeatsCount = repeats . config
  repeatsTxt _ = keyboard
  isWaitingForRepeats = waitingForRepeats
  setWaitingForRepeats wFr b = b {waitingForRepeats = wFr}
  setRepeatsCount r b@TelegramBot {config = c} = b {config = c {repeats = r}}
  tryGetRepeatsCount m = lookup (messText m) keyboardAnswers
    where
      keyboardAnswers = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5)]

keyboard :: String
keyboard =
  "\",\"reply_markup\": {\"keyboard\":[[\"1\",\"2\",\"3\",\"4\",\"5\"]],\"resize_keyboard\": true, \"one_time_keyboard\": true}}"

processUpdates :: Integer -> Either String Updates -> Maybe TelegramMessage
processUpdates lastId = either (const Nothing) (findLastMessage lastId . result)

findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage
findLastMessage oldId [] = Nothing
findLastMessage oldId (x:xs) =
  let mess = message x
      messId = message_id mess
   in if messId > oldId
        then Just mess
        else findLastMessage oldId xs
