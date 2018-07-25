module TestUtils where

import Control.Monad.Except (throwError)

checkResult :: String -> Bool -> Either String Bool
checkResult testName False = throwError $ "error : " ++ testName
checkResult _        True  = Right True