{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module EchoBot where

class (BotMessage m, BotConfig c) => EchoBot b m c | b -> m, b -> c where
  getBotWithConfig :: c -> b

  getLastMessage :: b -> IO m
  
  sendText :: String -> b -> IO ()

  sendRepeatsSelect :: b -> IO ()

  sendKeyboard :: b -> IO ()

class BotMessage m where
  getMessageText :: m -> String

  isRepeatsMessage :: m -> Bool
  isRepeatsMessage = (=="/repeat") . getMessageText

  isHelpMessage :: m -> Bool
  isHelpMessage = (=="/help") . getMessageText

class BotConfig c where
  getRepeatsCount :: c -> Int

  setRepeatsCount :: Int -> c -> c

  getHelpMessage :: c -> String