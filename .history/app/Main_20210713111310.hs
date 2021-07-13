{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import HttpHeadersPathDefinitions

main :: IO ()
main = do
  -- tc <- getTelegramSettings
  res <- getTelegramSettings
  case res of 
    Left msg -> do 
        responseBody <- msg
        pure $ Left $ Lib.Response 200 responseHeaders responseBody False
    Right tc -> runLambda (run tc)
  where
    responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
    run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
    run tc opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result