{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Monad.State
import Data.Maybe (fromMaybe, maybe)
import EchoBot
import SlackBot
import SlackConfig
import System.Environment
import TelegramBot
import TelegramConfig

main = do
  telegramConfig <- getTelegramConfig :: IO TelegramConfig
  slackConfig <- getSlackConfig :: IO SlackConfig
  async $ startEchoBot telegramConfig
  startEchoBot slackConfig

getTelegramConfig :: IO TelegramConfig
getTelegramConfig = do
  telegramToken <- fromMaybe undefined <$> lookupEnv "TG_TOKEN"
  telegramRepeats <- maybe 3 read <$> lookupEnv "TG_REPEATS"
  telegramHelp <-
    fromMaybe
      ("Echo bot. Repeats every message n times (default n = " ++
       show telegramRepeats ++ "). To change n write /repeat") <$>
    lookupEnv "TG_HELP"
  return $ TelegramConfig telegramToken telegramHelp telegramRepeats

getSlackConfig :: IO SlackConfig
getSlackConfig = do
  slackBotToken <- fromMaybe undefined <$> lookupEnv "SL_BOT_TOKEN"
  slackBotName <- fromMaybe undefined <$> lookupEnv "SL_BOT_NAME"
  slackAppToken <- fromMaybe undefined <$> lookupEnv "SL_APP_TOKEN"
  slackChannel <- fromMaybe undefined <$> lookupEnv "SL_CHANNEL"
  slackRepeats <- maybe 3 read <$> lookupEnv "SL_REPEATS"
  slackHelp <-
    fromMaybe
      ("Echo bot. Repeats every message n times (default n = " ++
       show slackRepeats ++ "). To change n write /repeat") <$>
    lookupEnv "SL_HELP"
  return $
    SlackConfig
      slackBotToken
      slackBotName
      slackAppToken
      slackChannel
      slackRepeats
      slackHelp

startEchoBot :: EchoBot b m c => c -> IO ()
startEchoBot = evalStateT runBot . getBotWithConfig

runBot :: EchoBot b m c => StateT b IO ()
runBot = do
  bot <- get
  m <- liftIO $ getLastMessage bot
  nBot <- liftIO $ maybe (return bot) (processMessage bot) m
  put nBot
  runBot
