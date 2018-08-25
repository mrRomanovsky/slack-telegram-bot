{-# LANGUAGE OverloadedStrings #-}

module Examples.Slack.Internal.Exceptions where

import Control.Exception
import qualified Data.ByteString.Lazy as B

handleGetException :: SomeException -> IO B.ByteString
handleGetException e = do
  appendFile "slack.log" $ "Caught exception while getting messages: " ++ show e
  return ""

handlePollException :: SomeException -> IO B.ByteString
handlePollException e = do
  appendFile "slack.log" $
    "Caught exception while getting poll answer: " ++ show e
  return ""

handleSendException :: SomeException -> IO ()
handleSendException e =
  appendFile "slack.log" $ "Caught exception while sending text: " ++ show e
