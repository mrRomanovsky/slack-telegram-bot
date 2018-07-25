import SlackTests
import TelegramTests
import TestUtils

main :: IO ()
main = do
  let tgTests = performUnitTests "Telegram Tests" telegramTests
      slTests = performUnitTests "Slack Tests" slackTests
  putStrLn tgTests
  putStrLn slTests
