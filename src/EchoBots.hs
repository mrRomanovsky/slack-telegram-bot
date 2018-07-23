{-# LANGUAGE MultiParamTypeClasses #-}

module EchoBots where

import EchoBot
import Control.Monad.State

echoMain :: EchoBot b m c => c -> IO ()
echoMain = evalStateT runBot . getBotWithConfig

runBot :: EchoBot b m c => StateT b IO ()
runBot = do
  bot <- get
  m <- getLastMessage--(getLastMessage :: StateT b IO (Maybe m))
  nBot <- liftIO $ maybe (return bot) (processMessage bot) m
  put nBot
  runBot

{-
  getBotWithConfig :: c -> b

  getLastMessage :: StateT b IO (Maybe m)

  processMessage :: m -> b -> IO b

-}