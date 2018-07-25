{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SlackBot where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe
import EchoBot
import Network.HTTP.Conduit hiding (httpLbs)
import Requests (postMessageSlack, sendSlack)
import SlackConfig
import SlackJson

data SlackBot = SlackBot
  { config :: SlackConfig
  , lastMessageTs :: String
  , waitingForRepeatsAnswer :: Bool
  , repeastsMessageTs :: String
  }

instance EchoBot SlackBot ValidSlackMessage SlackConfig where
  getBotWithConfig c = SlackBot c "" False ""
  getLastMessage sb@SlackBot {waitingForRepeatsAnswer = wr, config = c} =
    if wr
      then do
        let repeatsTst = repeastsMessageTs sb
        pollAnswer <- getPollAnswer sb
        getMessages c
        let repeatsCount = getRepeatsCount pollAnswer
        maybe
          (getLastTextMessage sb)
          (return . Just . RepeatsCount)
          repeatsCount
      else getLastTextMessage sb
  processMessage b@SlackBot {config = c} (RepeatsCount r) =
    return b {config = c {repeats = r}, waitingForRepeatsAnswer = False}
  processMessage b@SlackBot {config = c} (TextMessage m) =
    let lastTs = tStamp m
     in case validText m of
          "/help" ->
            sendText c (help c) >>
            return b {waitingForRepeatsAnswer = False, lastMessageTs = lastTs}
          "/repeat" -> do
            sendPoll c
            repeatsTs <- ts . head <$> getMessages c
            return
              b
                { waitingForRepeatsAnswer = True
                , lastMessageTs = lastTs
                , repeastsMessageTs = repeatsTs
                }
          txt ->
            replicateM_ (repeats c) (sendText c txt) >>
            return b {waitingForRepeatsAnswer = False, lastMessageTs = lastTs}

getLastTextMessage :: SlackBot -> IO (Maybe ValidSlackMessage)
getLastTextMessage sb@SlackBot {config = c} = do
  messagesInfo <- getMessages c
  return $ getLastUserMessage sb messagesInfo

getLastUserMessage :: SlackBot -> [SlackMessage] -> Maybe ValidSlackMessage
getLastUserMessage _ [] = Nothing
getLastUserMessage b@SlackBot {config = c, lastMessageTs = lastTs} (x:xs) =
  case parseTextMessage (botName c) lastTs x of
    (Just t) ->
      getLastUserMessage b xs <|>
      (Just $ TextMessage $ SlackTextMessage t $ ts x)
    _ -> getLastUserMessage b xs

parseTextMessage :: String -> String -> SlackMessage -> Maybe String
parseTextMessage bot lastTs SlackMessage { messageType = mt
                                         , user = u
                                         , text = t
                                         , ts = tStmp
                                         } =
  if isJust mt && isJust u && isJust t && tStmp > lastTs && fromJust u /= bot
    then parseMessageToBot ("<@" ++ bot ++ ">") $ fromJust t
    else Nothing

parseMessageToBot :: String -> String -> Maybe String
parseMessageToBot [] s = Just $ dropWhile isSpace s
parseMessageToBot (x:xs) [] = Nothing
parseMessageToBot (x:xs) (y:ys)
  | x == y = parseMessageToBot xs ys
  | otherwise = Nothing

sendText :: SlackConfig -> String -> IO ()
sendText sc@SlackConfig {botToken = t, channel = c} txt =
  catch
    (postMessageSlack
       "https://slack.com/api/chat.postMessage"
       (RequestBodyBS $
        pack $ "{\"channel\":\"" ++ c ++ "\",\"text\":\"" ++ txt ++ "\"}")
       t)
    handleSendException

getMessages :: SlackConfig -> IO [SlackMessage]
getMessages SlackConfig {appToken = t, channel = c} = do
  messagesStr <-
    catch
      (sendSlack "https://slack.com/api/channels.history" $
       "token=" ++ t ++ "&channel=" ++ c)
      handleGetException
  let messagesParsed =
        case messagesStr of
          "" -> Left "Didn't get an answer for request, but I'm still working!"
          msg -> eitherDecode msg :: Either String SlackResponse
  return $ either (const []) messages messagesParsed

sendPoll :: SlackConfig -> IO ()
sendPoll c =
  sendText c "Select repeats count:\n\n:one: :two: :three: :four: :five:"

getPollAnswer :: SlackBot -> IO (Either String ReactionsResponse)
getPollAnswer SlackBot { config = SlackConfig {botToken = t, channel = c}
                       , repeastsMessageTs = rTs
                       } = do
  answerStr <-
    catch
      (sendSlack "https://slack.com/api/reactions.get" $
       "token=" ++ t ++ "&channel=" ++ c ++ "&full=true&timestamp=" ++ rTs)
      handlePollException
  return (eitherDecode answerStr :: Either String ReactionsResponse)

getRepeatsCount :: Either String ReactionsResponse -> Maybe Int
getRepeatsCount = either (const Nothing) $ getAnswer . reactions . message
  where
    getAnswer = maybe Nothing $ parseAnswer . name . head
    parseAnswer "one" = Just 1
    parseAnswer "two" = Just 2
    parseAnswer "three" = Just 3
    parseAnswer "four" = Just 4
    parseAnswer "five" = Just 5
    parseAnswer _ = Nothing

handleGetException :: SomeException -> IO B.ByteString
handleGetException e = do
  writeFile "slack.log" $ "Caught exception while getting messages: " ++ show e
  return ""

handlePollException :: SomeException -> IO B.ByteString
handlePollException e = do
  writeFile "slack.log" $
    "Caught exception while getting poll answer: " ++ show e
  return ""

handleSendException :: SomeException -> IO ()
handleSendException e =
  writeFile "slack.log" $ "Caught exception while sending text: " ++ show e
