{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import Debug.Trace (trace)

{--
main :: IO ()
main = do
  res <- getTelegramSettings
  case res of 
    Left msg -> runLambda (runError msg)
    Right tc -> runLambda (run tc)
  where
    runError :: String -> LambdaOptions -> IO (Either String LambdaResult)
    runError s opts = do
      result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
    run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
    run tc opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
--}

main :: IO ()
main = do
  res <- getTelegramSettings
  case res of
    Left msg -> runLambda (run msg undefined)
    Right tc -> runLambda (run undefined tc)
  where
    run :: String -> TC -> LambdaOptions -> IO (Either String LambdaResult)
    run s tc opts
      | not Prelude.null s = do
          result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
      | otherwise = do 
          result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
