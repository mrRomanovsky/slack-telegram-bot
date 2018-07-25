module SlackTests where

import SlackBot
import SlackJson
import SlackConfig
import TestUtils
import Control.Monad.Except
import Data.Maybe (isNothing, isJust)

testGetLastUserMessage :: Either String Bool
testGetLastUserMessage = do
  let testName = "testGetLastUserMessage : "
  checkResult (testName ++ "test1") $
    isJust $ getLastUserMessage testBot [slackMess5]
  checkResult (testName ++ "test2") $
    isNothing $ getLastUserMessage testBot [slackMess1, slackMess2, slackMess3]
  checkResult (testName ++ "test3") $
    isNothing $ getLastUserMessage testBot []
  checkResult (testName ++ "test4") $
    isNothing $ getLastUserMessage testBot [slackMess5{ts = "0"}]
  checkResult (testName ++ "test5") $
    isJust $ getLastUserMessage testBot
             [slackMess5{ts = "0"}, slackMess5{ts = "2"}, slackMess5]

testBot = SlackBot testConfig "22" False "22"

testConfig = SlackConfig "bot" "bot" "app" "test" 3 "test"

messages = [slackMess1, slackMess2, slackMess3, slackMess4, slackMess5]

testParseTextMessage :: Either String Bool
testParseTextMessage = do
  let testName = "testParseTextMessage : "
  checkResult (testName ++ "test1") $
    isNothing $ parseTextMessage "bot" "0" slackMess1
  checkResult (testName ++ "test2") $
    isNothing $ parseTextMessage "bot" "0" slackMess2
  checkResult (testName ++ "test3") $
    isNothing $ parseTextMessage "bot" "0" slackMess3    
  checkResult (testName ++ "test4") $
    isNothing $ parseTextMessage "bot" "0" slackMess4
  checkResult (testName ++ "test5") $
    isJust $ parseTextMessage "bot" "40" slackMess1
  checkResult (testName ++ "test6") $
    isNothing $ parseTextMessage "bot" "43" slackMess1

slackMess1 = SlackMessage Nothing "42" Nothing Nothing
slackMess2 = slackMess1{messageType = Just "message"}
slackMess3 = slackMess2{user = Just "bot"}
slackMess4 = slackMess2{user = Just "user", text = Just "hello"}
slackMess5 = slackMess4{text = Just "<@bot>hello"}

testParseMessageToBot :: Either String Bool
testParseMessageToBot = do
  let testName = "testParseMessageToBot : "
  checkResult (testName ++ "test1") $
    isNothing $ parseMessageToBot "<@bot>" "hello"
  checkResult (testName ++ "test2") $
    parseMessageToBot "<@bot23>" "<@bot23>hello" ==
      Just "hello"
  checkResult (testName ++ "test3") $
    parseMessageToBot "<@bot42>" "<@bot42> " ==
      Just " "
  checkResult (testName ++ "test4") $
    isNothing $ parseMessageToBot "<@bot>" "<@bot12>hello"  

testGetRepeatsCount :: Either String Bool
testGetRepeatsCount = do
  let testName = "testGetRepeatsCount : "
  checkResult (testName ++ "test1") $
    isNothing $ getRepeatsCount $ Left "something went wrong"
  checkResult (testName ++ "test2") $
    isNothing $ getRepeatsCount $ Right reactResp1
  checkResult (testName ++ "test3") $
    getRepeatsCount (Right reactResp2) == Just 1
  checkResult (testName ++ "test4") $
    getRepeatsCount (Right reactResp3) == Just 2
  checkResult (testName ++ "test5") $
    getRepeatsCount (Right reactResp4) == Just 5
  checkResult (testName ++ "test6") $
    isNothing $ getRepeatsCount $ Right reactResp5

buildReactResponse :: Maybe [Reaction] -> ReactionsResponse
buildReactResponse = ReactionsResponse True . MessageReactionsInfo

[reactOne, reactTwo, reactThree, reactFour, reactFive, invalidReact] =
   Reaction <$> ["one", "two", "three", "four", "five", "invalid"]

[reactResp1, reactResp2, reactResp3, reactResp4, reactResp5] =
  buildReactResponse <$> [Nothing, Just [reactOne],
                          Just [reactTwo, reactOne],
                          Just [reactFive, reactTwo, reactThree],
                          Just [invalidReact]]