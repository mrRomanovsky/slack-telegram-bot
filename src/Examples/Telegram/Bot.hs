module Examples.Telegram.Bot
  ( getTelegramConfig
  , runTelegramEcho
  ) where

import Data.Maybe
import Echo.EchoBot
import Examples.Telegram.Internal.TelegramBot
import Examples.Telegram.Internal.TelegramConfig
import Logging.Config
import System.Environment
import Text.Read (readMaybe)

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
  logFile <- fromMaybe "telegram.log" <$> lookupEnv "TG_LOG_FILE"
  logConsoleStr <- fromMaybe "False" <$> lookupEnv "TG_LOG_CONSOLE"
  logLevelStr <- fromMaybe "Warning" <$> lookupEnv "TG_LOG_LEVEL"
  let logLevel =
        fromMaybe (error "incorrect log level!") $ readMaybe logLevelStr
      logConsole =
        fromMaybe (error "incorrect log_console parameter!") $
        readMaybe logConsoleStr
      logConfig = LogConfig logFile logLevel logConsole
  return $ TelegramConfig telegramToken telegramHelp telegramRepeats logConfig
