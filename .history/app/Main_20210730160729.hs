{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import Data.Text
import Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown)

import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )


main :: IO ()
main = do
    runLambdaHaskellRuntime 
      defaultDispatcherOptions
      (pure ()) 
      id $ do 
        addAPIGatewayHandler "api-gateway" gatewayHandler

determineResponse :: Maybe String -> Value -> Text
determineResponse t h 
  | t == Nothing = getTownNameWeatherFromIp (preProcessHeaders h)
  | otherwise = getTownNameWeatherFromTown (gettheTelegram t)

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler request context = do
    telegram <- getTelegram (apiGatewayRequestBody (request))
    responseBody <- determineResponse telegram (apiGatewayRequestHeaders (request)
    responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
    r <- getTelegramSettings
    case r of
      Left msg ->   "Fail : No Telegram Setting"
      Right tc ->   runTC tc $ handleUpdate responseBody telegram 
    response = pure $ Right $ ApiGatewayResponse 200 responseHeaders responseBody False
