{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import HttpHeadersPathDefinitions as H
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    eitherDecode,
    encode,
    object,
    (.:),
  )

main :: IO ()
main = do
  -- tc <- getTelegramSettings
  res <- getTelegramSettings
  case res of 
    Left msg -> runLambda (runError msg)
    Right tc -> runLambda (run tc)
  where
    runOther :: String -> LambdaOptions -> IO (Either String LambdaResult)
    runOther s opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
    run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
    run tc opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result