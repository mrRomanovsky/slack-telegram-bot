{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module EchoBot where

import Control.Monad.State

class EchoBot b m c | b -> m, b -> c, m -> b, c -> b, m -> c, c -> m where
  getBotWithConfig :: c -> b

  getLastMessage :: StateT b IO (Maybe m)

  processMessage :: b -> m -> IO b


{-
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
-}