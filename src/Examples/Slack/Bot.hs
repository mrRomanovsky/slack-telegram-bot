module Examples.Slack.Bot
  ( getSlackConfig
  , runSlackEcho
  ) where

import Bot.Bot
import Data.Maybe
import Echo.EchoBot
import Examples.Slack.Internal.SlackBot
import Examples.Slack.Internal.SlackConfig
import Logging.Config
import System.Environment
import Text.Read (readMaybe)

runSlackEcho :: SlackConfig -> IO ()
runSlackEcho = startEchoBot

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
  logFile <- fromMaybe "telegram.log" <$> lookupEnv "SL_LOG_FILE"
  logConsoleStr <- fromMaybe "False" <$> lookupEnv "SL_LOG_CONSOLE"
  logLevelStr <- fromMaybe "Warning" <$> lookupEnv "SL_LOG_LEVEL"
  let logLevel =
        fromMaybe (error "incorrect log level!") $ readMaybe logLevelStr
      logConsole =
        fromMaybe (error "incorrect log_console parameter!") $
        readMaybe logConsoleStr
      logConfig = LogConfig logFile logLevel logConsole
  return $
    SlackConfig
      slackBotToken
      slackBotName
      slackAppToken
      slackChannel
      slackRepeats
      slackHelp
      logConfig
