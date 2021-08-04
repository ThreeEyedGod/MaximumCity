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
    case res of
      Left msg -> do -- if we could not get Telegram Token
                  getTelegram body
                    | Nothing =  -> do
                      responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
                      pure $ Right $ H.Response 200 responseHeaders responseBody False
                    | otherwise = 
                      responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
                      pure $ Right $ H.Response 200 responseHeaders responseBody False
                  where
                      responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
      Right tc -> do
                  case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
                    Left _ -> do
                      responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
                      pure $ Right $ H.Response 200 responseHeaders responseBody False
                    Right update -> do
                        responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
                        runTC tc $ handleUpdate responseBody update 
                        pure $ Right $ H.Response 200 responseHeaders responseBody False
                  where
                      responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
  