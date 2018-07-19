module Testing where

import Control.Monad
import Network.HTTP.Conduit

botToken = "685994346:AAF1gb675pklyGI_6QC-wcS4xXMkmUQQ8dE"
chatId = "299903632"

botUrl = "https://api.telegram.org/bot" ++ botToken ++ "/"
keyboard = "&reply_markup={\"keyboard\":[[\"Yes\",\"No\"],[\"Maybe\"],[\"1\",\"2\",\"3\"]]}"

testMain :: IO ()
testMain = do
  let textToSend = "test" ++ keyboard 
      sendUrl = botUrl ++ "sendMessage?chat_id="
             ++ chatId ++ "&text=" ++ textToSend
  putStrLn sendUrl
  simpleHttp sendUrl
  return ()