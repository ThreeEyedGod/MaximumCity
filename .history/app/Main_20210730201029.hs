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

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler request context = do
    
    r :: ApiGatewayResponse "OK"
    r = ApiGatewayResponse {
      apiGatewayResponseStausCode = 200,
      apiGatewayResponseHeaders = ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)],
      
      apiGatewayResponseIsBase64Encoded = False
    }
    pure $ Right $ ApiGatewayResponse 200 responseHeaders responseBody False
    
