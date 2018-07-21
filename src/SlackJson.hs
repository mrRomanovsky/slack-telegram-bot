{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackJson where

import Data.Aeson
import GHC.Generics
import Control.Monad

data SlackResponse = 
  SlackResponse {ok :: Bool,
                 latest :: String,
                 messages :: [SlackMessage]} deriving (Show, Generic)

data SlackMessage = 
  SlackMessage {messageType :: String,
                ts :: String,
                user :: String,
                text :: String} deriving (Show, Generic)

instance FromJSON SlackMessage where
  parseJSON (Object v) =
     SlackMessage <$> v .: "type"
            <*> v .: "ts"
            <*> v .: "user"
            <*> v .: "text"
  parseJSON _ = mzero

instance ToJSON SlackMessage where
  toJSON (SlackMessage mType mTs mUser mText) =
    object ["type"  .= mType
          , "ts"   .= mTs
          , "user" .= mUser
          , "text" .= mText]

instance FromJSON SlackResponse
instance ToJSON SlackResponse
