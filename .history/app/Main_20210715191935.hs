{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import Debug.Trace (trace)

main :: IO ()
main = do
  res <- getTelegramSettings
  res <- getEnv "TELEGRAM_TOKEN"
  case res of
    Left msg -> runLambda (run msg undefined)
    Right tc -> runLambda (run "" tc)
  where
    run :: String -> TC -> LambdaOptions -> IO (Either String LambdaResult)
    run s tc opts
      | not (Prelude.null s) = do
          result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
      | otherwise = do 
          result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
