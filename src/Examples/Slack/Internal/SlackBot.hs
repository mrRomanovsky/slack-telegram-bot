{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Slack.Internal.SlackBot where

import Bot.Bot
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
import Echo.EchoBot
import Examples.Slack.Internal.Exceptions
import Examples.Slack.Internal.Requests
import Examples.Slack.Internal.SlackConfig
import Examples.Slack.Internal.SlackJson
import Network.HTTP.Conduit hiding (httpLbs)

data SlackBot = SlackBot
  { config :: SlackConfig
  , lastMessageTs :: String
  , waitingForRepeatsAnswer :: Bool
  , repeastsMessageTs :: String
  }

instance Bot SlackBot where
  type BotConfig SlackBot = SlackConfig
  type BotMessage SlackBot = ValidSlackMessage
  getBotWithConfig c = SlackBot c "" False ""
  getLastMessage = do
    sb <- get
    msg <-
      if waitingForRepeatsAnswer sb
        then do
          repCntAnsw <- liftIO $ getPollAnswer sb
          case getRepeatsCount repCntAnsw of
            (Just r) -> return $ Just $ RepeatsCount r
            _ -> liftIO $ getLastTextMessage sb
        else liftIO $ getLastTextMessage sb
    put $ maybe sb (`newTs` sb) msg
    return msg
  sendMessageTo _ msg = do
    sb <- get
    liftIO $ sendText (config sb) msg

newTs :: ValidSlackMessage -> SlackBot -> SlackBot
newTs msg sb =
  case messId msg of
    "" -> sb
    tSt -> sb {lastMessageTs = tSt}

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

getRepeatsCount :: Either String ReactionsResponse -> Maybe String
getRepeatsCount =
  either (const Nothing) $ fmap (name . head) . reactions . message

getLastTextMessage :: SlackBot -> IO (Maybe ValidSlackMessage)
getLastTextMessage sb@SlackBot {config = c, waitingForRepeatsAnswer = wFr} = do
  messagesInfo <- getMessages c
  return $ getLastUserMessage sb messagesInfo

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

instance EchoBot SlackBot where
  helpMessage SlackBot {config = c} = help c
  repeatsCount = repeats . config
  repeatsTxt _ = "Select repeats count:\n\n:one: :two: :three: :four: :five:"
  isWaitingForRepeats = waitingForRepeatsAnswer
  setWaitingForRepeats b sb = sb {waitingForRepeatsAnswer = b}
  setRepeatsCount r sb@SlackBot {config = c} = sb {config = c {repeats = r}}
  tryGetRepeatsCount (TextMessage _) = Nothing
  tryGetRepeatsCount (RepeatsCount rs) = parseRepeats rs

parseRepeats "one" = Just 1
parseRepeats "two" = Just 2
parseRepeats "three" = Just 3
parseRepeats "four" = Just 4
parseRepeats "five" = Just 5
parseRepeats _ = Nothing
