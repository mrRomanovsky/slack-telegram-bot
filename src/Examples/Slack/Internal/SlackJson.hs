{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Slack.Internal.SlackJson where

import Bot.Message
import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.Text.Internal as T
import GHC.Generics

data SlackResponse = SlackResponse
  { ok :: Bool
  , messages :: [SlackMessage]
  , has_more :: Bool
  } deriving (Show, Generic)

data SlackMessage = SlackMessage
  { messageType :: Maybe String
  , ts :: String
  , user :: Maybe String
  , text :: Maybe String
  } deriving (Show, Generic)

instance FromJSON SlackMessage where
  parseJSON (Object v) =
    SlackMessage <$> v .:? "type" <*> v .: "ts" <*> v .:? "user" <*>
    v .:? "text"
  parseJSON _ = mzero

data ReactionsResponse = ReactionsResponse
  { reactionsOk :: Bool
  , message :: MessageReactionsInfo
  } deriving (Show, Generic)

instance FromJSON ReactionsResponse where
  parseJSON (Object v) = ReactionsResponse <$> v .: "ok" <*> v .: "message"

newtype MessageReactionsInfo = MessageReactionsInfo
  { reactions :: Maybe [Reaction]
  } deriving (Show, Generic)

newtype Reaction = Reaction
  { name :: String
  } deriving (Show, Generic)

instance FromJSON MessageReactionsInfo

instance FromJSON SlackResponse

instance FromJSON Reaction

mField :: ToJSON a => T.Text -> Maybe a -> Maybe (T.Text, Value)
mField field = fmap ((field .=) . toJSON)
