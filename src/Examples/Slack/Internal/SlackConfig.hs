{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.Slack.Internal.SlackConfig where

import Bot.Message
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
  | RepeatsCount String deriving (Show)

data SlackTextMessage = SlackTextMessage
  { validText :: String
  , tStamp :: String
  } deriving (Show)

instance Message ValidSlackMessage where
  type Id ValidSlackMessage = String
  messId (TextMessage m) = tStamp m
  messId _ = ""
  messText (TextMessage m) = validText m
  messText (RepeatsCount rc) = rc

getTs :: ValidSlackMessage -> String
getTs (TextMessage m) = tStamp m
getTs _ = ""

instance FromJSON SlackConfig
