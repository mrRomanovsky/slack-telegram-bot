module Examples.Telegram.Internal.Exceptions where

import Control.Exception

handleException :: SomeException -> IO ()
handleException e =
  appendFile "telegram.log" $
  "Caught exception while sending message: " ++ show e
