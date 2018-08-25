module Examples.Telegram.Bot
  ( getTelegramConfig
  , runTelegramEcho
  ) where

import Data.Maybe
import Echo.EchoBot
import Examples.Telegram.Internal.TelegramBot
import Examples.Telegram.Internal.TelegramConfig
import System.Environment

runTelegramEcho :: TelegramConfig -> IO ()
runTelegramEcho = startEchoBot

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
