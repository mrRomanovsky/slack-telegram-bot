module TestUtils
  ( checkResult
  , performUnitTests
  ) where

import Control.Monad.Except

checkResult :: String -> Bool -> Either String Bool
checkResult testName False = throwError $ "error : " ++ testName
checkResult _ True = Right True

testResult :: (String, Either String Bool) -> String
testResult (testName, Right b) = "\t" ++ testName ++ " passed : " ++ show b
testResult (testName, Left err) = "\t" ++ testName ++ " failed : " ++ err

performUnitTests :: String -> [(String, Either String Bool)] -> String
performUnitTests moduleName tests =
  unlines $ moduleName : (testResult <$> tests)
