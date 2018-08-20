module Examples.Slack.Bot
  ( getSlackConfig
  , runSlackEcho
  ) where

import Bot.Bot
import Data.Maybe
import Echo.EchoBot
import Examples.Slack.Internal.SlackBot
import Examples.Slack.Internal.SlackConfig
import System.Environment

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
  return $
    SlackConfig
      slackBotToken
      slackBotName
      slackAppToken
      slackChannel
      slackRepeats
      slackHelp
