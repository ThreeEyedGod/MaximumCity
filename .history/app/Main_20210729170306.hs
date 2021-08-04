{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
--import Debug.Trace (trace)

main :: IO ()
main = do
  --res <- getTelegramSettings
  --case res of
    --Left msg -> runLambda (runLH msg undefined) -- if we could not get Telegram Token
    --Right tc -> runLambda (runLH "" tc)
    thisCall :: IO context
    runLambda thisCall runCB
  where
    runCB :: APIGatewayHandlerType (LambdaOptions context) -> IO (Either LambdaError LambdaResult)
    runCB opts = do
        result <- handlerTelegram (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
        either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result

    {--runLH :: String -> TC -> (LambdaOptions context) -> IO (Either LambdaError LambdaResult)
    runLH s tc opts
      | not (Prelude.null s) = do -- no telegram token. Route it to bad handler
          result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result
      | otherwise = do -- OK case regular handler will deal with it
          result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result
    --}

handlerTelegram :: Event -> Context context -> IO (Either String H.Response)
handlerTelegram Event {path, headers, body} context = 
  do
    res <- getTelegramSettings
    case res of
      Left msg -> do -- if we could not get Telegram Token
                  case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
                    Left _ -> do
        responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
        pure $ Right $ H.Response 200 responseHeaders responseBody False
      Right update -> do
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
  