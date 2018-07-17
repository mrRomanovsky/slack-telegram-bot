{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramJson where
import Data.Aeson
import GHC.Generics
import Control.Monad

data Updates = Updates { ok     :: Bool,
                         result :: [Update] } deriving (Show, Generic)

data Update = Update { update_id :: Integer,
                       message   :: Message } deriving (Show, Generic)

data Message = Message { message_id :: Integer,
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

instance ToJSON User where
  toJSON (User userId firstName) =
    object ["id"  .= userId
           , "first_name"   .= firstName]

data Chat = Chat { chat_id :: Integer,
                   chat_first_name :: String,
                   chat_type :: String } deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON (Object v) =
     Chat <$> v .: "id"
            <*> v .: "first_name"
            <*> v .: "type"
  parseJSON _ = mzero

instance ToJSON Chat where
  toJSON (Chat chatId chatFirstName chatType) =
    object [ "id"  .= chatId
           , "first_name" .= chatFirstName
           , "type"   .= chatType]

instance FromJSON Updates
instance FromJSON Update
instance FromJSON Message
instance ToJSON Updates
instance ToJSON Update
instance ToJSON Message