{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackJson where

import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.Maybe
import qualified Data.Text.Internal as T

data SlackResponse = 
  SlackResponse {ok :: Bool,
                 messages :: [SlackMessage],
                 has_more :: Bool} deriving (Show, Generic)

data SlackMessage = 
  SlackMessage {messageType :: Maybe String,
                ts :: String,
                user :: Maybe String,
                text :: Maybe String} deriving (Show, Generic)

instance FromJSON SlackMessage where
  parseJSON (Object v) =
     SlackMessage <$> v .:? "type"
            <*> v .: "ts"
            <*> v .:? "user"
            <*> v .:? "text"
  parseJSON _ = mzero

instance ToJSON SlackMessage where --maybe I don't need it at all?
  toJSON (SlackMessage mType mTs mUser mText) =
    object $ catMaybes [mField "type" mType,
                        mField "ts" $ Just mTs,
                        mField "user" mUser,
                        mField "text" mText]

data ReactionsResponse = 
  ReactionsResponse { reactionsOk :: Bool
                   , message :: MessageReactionsInfo} deriving (Show, Generic)

instance FromJSON ReactionsResponse where
  parseJSON (Object v) = 
    ReactionsResponse <$> v .: "ok"
          <*> v .: "message"

data MessageReactionsInfo = 
  MessageReactionsInfo {reactions :: Maybe [Reaction]} deriving (Show, Generic) -- maybe turn it into newtype?

data Reaction = Reaction {name :: String} deriving (Show, Generic) --maybe turn it into newtype?

instance FromJSON MessageReactionsInfo
instance FromJSON SlackResponse
instance FromJSON Reaction
instance ToJSON SlackResponse

mField :: ToJSON a => T.Text -> Maybe a -> Maybe (T.Text, Value)
mField field = fmap ((field .=) . toJSON)

{-catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter iJust-}