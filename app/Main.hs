{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import EchoBot
import Control.Monad.State
import TelegramConfig
import SlackConfig
import System.Environment
import Data.Maybe (fromMaybe, maybe)

main = do
  telegramConfig <- getTelegramConfig
  slackConfig <- getSlackConfig
  return ()

getTelegramConfig :: IO TelegramConfig
getTelegramConfig = do
  telegramToken <- fromMaybe undefined <$> lookupEnv "TG_TOKEN"
  telegramRepeats <- maybe 3 read <$> lookupEnv "TG_REPEATS"
  telegramHelp <- fromMaybe ("Echo bot. Repeats every message n times (default n = "
   ++ show telegramRepeats ++ "). To change n write /repeat") <$> lookupEnv "TG_HELP"
  return $ TelegramConfig telegramToken telegramHelp telegramRepeats

getSlackConfig :: IO SlackConfig
getSlackConfig = do
  slackBotToken <- fromMaybe undefined <$> lookupEnv "SL_BOT_TOKEN"
  slackBotName <- fromMaybe undefined <$> lookupEnv "SL_BOT_NAME"
  slackAppToken <- fromMaybe undefined <$> lookupEnv "SL_APP_TOKEN"
  slackChannel <- fromMaybe undefined <$> lookupEnv "SL_CHANNEL"
  slackRepeats <- maybe 3 read <$> lookupEnv "SL_REPEATS"
  slackHelp <- fromMaybe ("Echo bot. Repeats every message n times (default n = "
   ++ show slackRepeats ++ "). To change n write /repeat") <$> lookupEnv "SL_HELP"
  return $ SlackConfig slackBotToken slackBotName slackAppToken slackChannel slackRepeats slackHelp

echoMain :: EchoBot b m c => c -> IO ()
echoMain = evalStateT runBot . getBotWithConfig

runBot :: EchoBot b m c => StateT b IO ()
runBot = do
  bot <- get
  m <- getLastMessage
  nBot <- liftIO $ maybe (return bot) (processMessage bot) m
  put nBot
  runBot

{-myTelegramConfig = TelegramConfig "685994346:AAF1gb675pklyGI_6QC-wcS4xXMkmUQQ8dE"
 "Echo bot. Repeats every message n times (default : 3). To change n write /repeat"
 3-}