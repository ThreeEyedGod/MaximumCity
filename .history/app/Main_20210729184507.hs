{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
--import Debug.Trace (trace)
import HttpHeadersPathDefinitions as H
import Data.Text
import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )


main :: IO ()
main = do
    thisCall :: IO context
    runLambda thisCall runCB
  where
    runCB :: LambdaOptions context -> IO (Either LambdaError LambdaResult)
    runCB opts = do
        result <- handlerTelegram (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
        either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result

determineResponse :: Maybe String -> Value -> Text
determineResponse t h 
  | t == Nothing = getTownNameWeatherFromIp (preProcessHeaders h)
  | otherwise = getTownNameWeatherFromTown (gettheTelegram t)

handlerTelegram :: Event -> Context context -> IO (Either String H.Response)
handlerTelegram Event {path, headers, body} context = 
  do
    r <- getTelegramSettings
    telegram <- getTelegram body
    responseBody <- determineResponse telegram headers
    pure $ Right $ H.Response 200 responseHeaders responseBody False
    case r of
      Left msg ->   "Fail : No Telegram Setting"
      Right tc ->   runTC tc $ handleUpdate responseBody update 
  where
      responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
  