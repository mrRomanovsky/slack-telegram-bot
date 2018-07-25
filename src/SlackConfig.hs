{-# LANGUAGE DeriveGeneric #-}

module SlackConfig where

import Data.Aeson
import GHC.Generics

data SlackConfig = SlackConfig
  { botToken :: String
  , botName :: String
  , appToken :: String
  , channel :: String
  , repeats :: Int
  , help :: String
  } deriving (Show, Generic)

data ValidSlackMessage
  = TextMessage SlackTextMessage
  | RepeatsCount Int

data SlackTextMessage = SlackTextMessage
  { validText :: String
  , tStamp :: String
  }

getTs :: String -> ValidSlackMessage -> String
getTs _ (TextMessage m) = tStamp m
getTs def _ = ""

instance FromJSON SlackConfig
