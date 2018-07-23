{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module EchoBot where

import Control.Monad.State

class EchoBot b m c | b -> m, b -> c where
  getBotWithConfig :: c -> b

  getLastMessage :: StateT b IO (Maybe m)

  processMessage :: m -> b -> IO ()


{-
checkUpdates :: TelegramConfig -> String -> String -> StateT Integer IO ()
checkUpdates c getUpdates botUrl = do
  oldId <- get
  updatesStr <- liftIO $ catch (simpleHttp getUpdates) $ return . handleHttpException
  let updates = case updatesStr of
        "" -> Left "Didn't get an answer for request, but I'm still working!"
        upd -> eitherDecode upd :: Either String Updates
  maybe (checkUpdates c getUpdates botUrl) (\mInfo -> do
    let msg = mess mInfo
    if wasRepeat mInfo
       then checkUpdates c{repeats = getRepeats msg} getUpdates botUrl
       else do
         newId <- liftIO $ sendMessage c botUrl msg --maybe I should build request strings only once, somewhere above
         put newId
         checkUpdates c getUpdates botUrl) (processUpdates c botUrl oldId updates)
-}


{-
class BotMessage m where
  getMessageText :: m -> String

  isRepeatsMessage :: m -> Bool
  isRepeatsMessage = (=="/repeat") . getMessageText

  isHelpMessage :: m -> Bool
  isHelpMessage = (=="/help") . getMessageText

class BotConfig c where
  getRepeatsCount :: c -> Int

  setRepeatsCount :: Int -> c -> c

  getHelpMessage :: c -> String
-}