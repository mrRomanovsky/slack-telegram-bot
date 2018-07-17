{-# LANGUAGE DeriveGeneric #-}

module Config where
import Data.Text
import Data.Aeson

import GHC.Generics
data Config = Config { token   :: !Text,
                       repeats :: Int} deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config