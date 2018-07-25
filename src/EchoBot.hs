{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module EchoBot where

import Control.Monad.State

class EchoBot b m c | b -> m c, c -> b m, m -> b c, c -> b, c -> m where
  getBotWithConfig :: c -> b
  getLastMessage :: b -> IO (Maybe m) --will be used for logging
  processMessage :: b -> m -> IO b
