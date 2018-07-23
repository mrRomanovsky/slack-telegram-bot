{-# LANGUAGE MultiParamTypeClasses #-}

module EchoBots where

import EchoBot
import Control.Monad.State
import TelegramConfig

echoMain :: EchoBot b m c => c -> IO ()
echoMain = evalStateT runBot . getBotWithConfig

runBot :: EchoBot b m c => StateT b IO ()
runBot = do
  bot <- get
  m <- getLastMessage
  nBot <- liftIO $ maybe (return bot) (processMessage bot) m
  put nBot
  runBot

myTelegramConfig = TelegramConfig "685994346:AAF1gb675pklyGI_6QC-wcS4xXMkmUQQ8dE"
 "Echo bot. Repeats every message n times (default : 3). To change n write /repeat"
 3