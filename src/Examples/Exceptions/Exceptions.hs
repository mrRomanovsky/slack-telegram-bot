{-# LANGUAGE OverloadedStrings #-}

module Examples.Exceptions.Exceptions where

import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import Logging.Config

handleGetException :: LogConfig -> SomeException -> IO B.ByteString
handleGetException LogConfig { logFile = logF
                             , logLevel = logL
                             , logConsole = logCons
                             } e = do
  when
    (logL <= Warning)
    (do let errMess =
              "Warning: server did not send information about new messages: they may be lost: " ++
              show e
        when logCons (print errMess)
        appendFile logF errMess)
  return ""

handlePollException :: LogConfig -> SomeException -> IO B.ByteString
handlePollException LogConfig { logFile = logF
                              , logLevel = logL
                              , logConsole = logCons
                              } e = do
  let errMess = "Caught exception while getting poll answer: " ++ show e
  when logCons (print errMess)
  appendFile logF errMess
  return ""

handleSendException :: LogConfig -> SomeException -> IO ()
handleSendException LogConfig { logFile = logF
                              , logLevel = logL
                              , logConsole = logCons
                              } e = do
  let errMess = "Caught exception while sending message: " ++ show e
  when logCons (print errMess)
  appendFile logF errMess
