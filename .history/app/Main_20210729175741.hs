{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
--import Debug.Trace (trace)

main :: IO ()
main = do
    thisCall :: IO context
    runLambda thisCall runCB
  where
    runCB :: APIGatewayHandlerType (LambdaOptions context) -> IO (Either LambdaError LambdaResult)
    runCB opts = do
        result <- handlerTelegram (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
        either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result

handlerTelegram :: Event -> Context context -> IO (Either String H.Response)
handlerTelegram Event {path, headers, body} context = 
  do
    res <- getTelegramSettings
    telegram <- getTelegram body
    pure $ Right $ H.Response 200 responseHeaders responseBody False
    case res of
      Left msg -> do -- if we could not get Telegram Token
                    case telegram of 
                      | Nothing ->  
                          responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
                      | otherwise -> 
                          responseBody <- getTownNameWeatherFromTown (gettheTelegram telegram)
                  where
                      responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
      Right tc -> do
                    case telegram of
                      | Nothing -> 
                          responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
                      | otherwise -> 
                          responseBody <- getTownNameWeatherFromTown (gettheTelegram telegram)
                          runTC tc $ handleUpdate responseBody update 
  where
      responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
  