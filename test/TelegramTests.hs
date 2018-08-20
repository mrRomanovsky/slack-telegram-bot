module TelegramTests
  ( telegramTests
  ) where

import Control.Monad.Except
import Data.Maybe (isNothing)
import Examples.Telegram.Internal.TelegramBot
import Examples.Telegram.Internal.TelegramConfig
import Examples.Telegram.Internal.TelegramJson
import TestUtils

telegramTests =
  [ ("testFindLastMessage", testFindLastMessage)
  , ("testProcessUpdates", testProcessUpdates)]

testProcessUpdates :: Either String Bool
testProcessUpdates = do
  let testName = "testProcessUpdates : "
      upds = Updates True updates
  checkResult (testName ++ "test1") $
    isNothing $ processUpdates 1 $ Left "something went wrong"
  checkResult (testName ++ "test2") $
    maybe False ((== 2) . message_id) $ processUpdates 1 $ Right upds
  checkResult (testName ++ "test3") $
    maybe False ((== 23) . message_id) $ processUpdates 4 $ Right upds
  checkResult (testName ++ "test2") $ isNothing $ processUpdates 23 $ Right upds

testFindLastMessage :: Either String Bool
testFindLastMessage = do
  let testName = "testFindLastMessage : "
  checkResult (testName ++ "test1") $
    maybe False ((== 23) . message_id) $ findLastMessage 4 updates
  checkResult (testName ++ "test2") $
    maybe False ((== 1) . message_id) $ findLastMessage 0 updates
  checkResult (testName ++ "test3") $
    maybe False ((== 2) . message_id) $ findLastMessage 1 updates
  checkResult (testName ++ "test4") $ isNothing $ findLastMessage 23 updates

buildUpdate :: Integer -> Update
buildUpdate n = Update n $ TelegramMessage n testUser testChat 23 "test"

updates :: [Update]
updates = buildUpdate <$> [1, 2, 23, 5]

testingBot1 = TelegramBot testingConfig1 "test" "test" "test" 0 False

testingConfig1 = TelegramConfig "test" "test" 4

testChat = Chat 42 "test" "test"

testUser = User 42 "test"
