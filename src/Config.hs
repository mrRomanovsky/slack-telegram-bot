{-# LANGUAGE DeriveGeneric #-}

module Config where
import Data.Aeson
import GHC.Generics

data Config = Config { token   :: String,
                       repeats :: Int} deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config