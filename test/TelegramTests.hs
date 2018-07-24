module TelegramTests where

import TelegramBot
import TelegramConfig
import Control.Monad.Except
{-
data TelegramBot = TelegramBot { config :: TelegramConfig
                               , botUrl :: String
                               , getUpdates :: String
                               , sendMessage :: String
                               , lastMessId :: Integer
                               , waitingForRepeats :: Bool }

data TelegramConfig = TelegramConfig { token   :: String,
                       help    :: String,
                       repeats :: Int} deriving (Show, Generic)

data Updates = Updates { ok     :: Bool,
                         result :: [Update] } deriving (Show, Generic)

data Update = Update { update_id :: Integer,
                       message   :: TelegramMessage } deriving (Show, Generic)

data TelegramMessage = TelegramMessage { message_id :: Integer,
                         from :: User,
                         chat :: Chat,
                         date :: Integer,
                         text :: String } deriving (Show, Generic)
-}

testChangeRepeats :: Either String Bool
testChangeRepeats = do
  checkResult "testChangeRepeats : test1" $ repeats (config $ changeRepeats 4 testingBot1) == 4
  checkResult "testChangeRepeats : test2" $ repeats (config $ changeRepeats 2 testingBot1) == 2
  checkResult "testChangeRepeas : test3" $ repeats (config $ changeRepeats 0 testingBot1) == 0

testProcessUpdates :: Either String Bool
testProcessUpdates = Right False

testFindLastMessage :: Either String Bool
testFindLastMessage = Right False

checkResult :: String -> Bool -> Either String Bool
checkResult testName False = throwError $ "error : " ++ testName
checkResult _        True  = Right True

testingBot1 = TelegramBot testingConfig1 "test" "test" "test" 0 False

testingConfig1 = TelegramConfig "test" "test" 4

{-

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