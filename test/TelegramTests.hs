module TelegramTests where

import TelegramBot
import TelegramConfig

testChangeRepeats :: Bool
testChangeRepeats = False

testProcessUpdates :: Bool
testProcessUpdates = False

testFindLastMessage :: Bool
testFindLastMessage = False
{-
changeRepeats :: TelegramBot -> Integer -> Int -> TelegramBot
changeRepeats b@TelegramBot{config = c} newId r =
  b{lastMessId = newId, config = c{repeats = r}, waitingForRepeats = False}

sendText :: String -> Integer -> String -> IO ()
sendText txt chatId sendUrl = send sendUrl
 (RequestBodyBS $ pack $ "{\"chat_id\": "++ show chatId ++
  ",\"text\": \"" ++ txt)

processUpdates :: TelegramConfig -> Integer -> Either String Updates -> Maybe TelegramMessage
processUpdates c lastId = either (const Nothing) (findLastMessage lastId . result)

findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage
findLastMessage oldId [] = Nothing
findLastMessage oldId (x:xs) = let mess = message x
                                   messId = message_id mess
                                   in if messId > oldId
                                         then Just mess
                                         else findLastMessage oldId xs

-}