module Main where

import Control.Concurrent.Async
import Echo.EchoBot
import Examples.Slack.Bot
import Examples.Telegram.Bot

main = do
  telegramConfig <- getTelegramConfig
  runTelegramEcho telegramConfig
  --slackConfig <- getSlackConfig
-- async $ runTelegramEcho telegramConfig
--  runSlackEcho slackConfig