module SlackTests where

testGetLastUserMessage :: Bool
testGetLastUserMessage = False

testParseTextMessage :: Bool
testParseTextMessage = False

testParseMessageToBot :: Bool
testParseMessageToBot = False

testGetRepeatsCount :: Bool
testGetRepeatsCount = False
{-

getLastUserMessage :: SlackBot -> [SlackMessage] -> Maybe ValidSlackMessage
getLastUserMessage _ [] = Nothing
getLastUserMessage b@SlackBot{config = c, lastMessageTs = lastTs} (x:xs) = 
  case parseTextMessage (botName c) lastTs x of
    (Just t) -> getLastUserMessage b xs <|> (Just $ TextMessage $ SlackTextMessage t $ ts x)
    _        -> getLastUserMessage b xs

parseTextMessage :: String -> String -> SlackMessage -> Maybe String
parseTextMessage bot lastTs SlackMessage{messageType = mt, user = u, text = t, ts = tStmp} =
  if isJust mt && isJust u && isJust t && tStmp > lastTs && fromJust u /= bot
     then parseMessageToBot ("<@" ++ bot ++">") $ fromJust t
     else Nothing
              
parseMessageToBot :: String -> String -> Maybe String
parseMessageToBot [] s = Just $ dropWhile isSpace s
parseMessageToBot (x:xs) [] = Nothing
parseMessageToBot (x:xs) (y:ys)
  |x == y = parseMessageToBot xs ys
  |otherwise = Nothing

getRepeatsCount :: Either String ReactionsResponse -> Maybe Int
getRepeatsCount = either (const Nothing) $ getAnswer . reactions . message
  where
    getAnswer = maybe Nothing $ parseAnswer . name . head
    parseAnswer "one" = Just 1
    parseAnswer "two" = Just 2
    parseAnswer "three" = Just 3
    parseAnswer "four" = Just 4
    parseAnswer "five" = Just 5

-}