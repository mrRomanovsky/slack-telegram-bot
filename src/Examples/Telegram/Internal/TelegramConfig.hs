{-# LANGUAGE DeriveGeneric #-}

module Examples.Telegram.Internal.TelegramConfig where

import Data.Aeson
import GHC.Generics

data TelegramConfig = TelegramConfig
  { token :: String
  , help :: String
  , repeats :: Int
  } deriving (Show, Generic)

instance FromJSON TelegramConfig

instance ToJSON TelegramConfig
