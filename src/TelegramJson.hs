{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramJson where
import Data.Aeson
import GHC.Generics
import Control.Monad

data Updates = Updates { ok     :: Bool,
                         result :: [Update] } deriving (Show, Generic)

data Update = Update { update_id :: Integer,
                       message   :: TelegramMessage } deriving (Show, Generic)

data TelegramMessage = TelegramMessage { message_id :: Integer,
                         from :: User,
                         chat :: Chat,
                         date :: Integer,
                         text :: String } deriving (Show, Generic)

data User = User { user_id :: Integer,
                   first_name :: String } deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) =
     User <$> v .: "id"
            <*> v .: "first_name"
  parseJSON _ = mzero

data Chat = Chat { chat_id :: Integer,
                   chat_first_name :: String,
                   chat_type :: String } deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON (Object v) =
     Chat <$> v .: "id"
            <*> v .: "first_name"
            <*> v .: "type"
  parseJSON _ = mzero

instance FromJSON Updates
instance FromJSON Update
instance FromJSON TelegramMessage