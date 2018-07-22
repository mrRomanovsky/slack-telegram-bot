{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE RecordWildCards #-}

module SlackBot where

import SlackJson
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
import Lib
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Control.Monad.State
import GHC.Generics
import Control.Exception
import Data.Maybe

data SlackConfig = SlackConfig {token :: String,
                                channel :: String} deriving (Show, Generic)

botName = "BOT_NAME"

instance FromJSON SlackConfig
instance ToJSON SlackConfig

echoLastMessage :: SlackConfig -> IO ()
echoLastMessage sc = do
  lastMessages <- getMessages sc
  let lastText = maybe Nothing  getLastMessageText lastMessages
  putStrLn "lastMessages :"
  print lastMessages
  putStrLn "lastText :"
  print lastText
  maybe (return ()) (sendText sc) lastText


--add timestamp processing
getLastMessageText :: [SlackMessage] -> Maybe String
getLastMessageText [] = Nothing
getLastMessageText (x:xs) = 
  maybe (getLastMessageText xs) Just $ getUserMessage x

--maybe I can just check text?
getUserMessage :: SlackMessage -> Maybe String
getUserMessage SlackMessage{messageType = mt, user = u, text = t} = 
  if isJust mt && isJust u && isJust t then parseMessageToBot $ fromJust t
                                       else Nothing

parseMessageToBot :: String -> Maybe String
parseMessageToBot str = reverse <$> res
  where
    (res, _, _) = foldl startBot (Just "", "", botName) str
    startBot acc@(Nothing, _, _) _ = acc
    startBot (n, "", botName) '<' = (n, "<", botName)
    startBot (n, "<", botName) '@' = (n, "<@", botName)
    startBot (n, "<@", []) '>' = (n, "<@>", [])
    startBot (n, "<@", (x:xs)) c
      |c == x = (n, "<@", xs)
      |otherwise = (Nothing, "<@", (x:xs))
    startBot (Just s, "<@>", []) x = (Just $ x:s, "<@>", [])
    startBot (n, p, bn) _ = (Nothing, p, bn)

sendText :: SlackConfig -> String -> IO ()
sendText sc@SlackConfig{token = t, channel = c} txt =
  postMessageSlack "https://slack.com/api/chat.postMessage" (RequestBodyBS $ pack $
   "{\"channel\":\"" ++ c ++ "\",\"text\":\"" ++ txt ++ "\"}") "xoxb-403965130790-402459105700-tTSoVZoJH6TTfDb51QhkUiC5"

getMessages :: SlackConfig -> IO (Maybe [SlackMessage])
getMessages SlackConfig{token = t, channel = c} = do
  messagesStr <- catch (sendSlack "https://slack.com/api/channels.history" $
    "token=" ++ t ++ "&channel=" ++ c) $ return . handleHttpException
  --print messagesStr
  let messagesParsed = case messagesStr of
        "" -> Left "Didn't get an answer for request, but I'm still working!"
        msg -> eitherDecode msg :: Either String SlackResponse
  return $ either (const Nothing) (Just . messages) messagesParsed

sendPoll :: SlackConfig -> IO ()
sendPoll c = sendText c "Select repeats count:\n\n:one: :two: :three: :four: :five:"

getPollAnswer :: SlackConfig -> IO Int
getPollAnswer SlackConfig{token = t, channel = c} = do
  answerStr <- catch (sendSlack "https://slack.com/api/reactions.get" $
    "token=" ++ botName ++ "&channel=" ++ c ++
     "&full=true&timestamp=1532275649.000040") $ return . handleHttpException
  print answerStr
  return (-1)

handleHttpException :: SomeException -> B.ByteString --add normal exception handling
handleHttpException e = "Something went wrong"

myConfig = SlackConfig "MY_TOKEN" "MY_CHAT_ID"
