{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  putStrLn "hello world"
  strConf <- B.readFile "app/config/config.txt"
  let conf = eitherDecode strConf :: Either String Config
  either putStrLn print conf