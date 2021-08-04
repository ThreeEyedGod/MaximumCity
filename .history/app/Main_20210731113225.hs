{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude
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
main = runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) 
          id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler request context = do
    let (b :: ApiGatewayResponseBody) = "OK"
    let r = ApiGatewayResponse b { 
                    apiGatewayResponseStatusCode = 200,
                    apiGatewayResponseHeaders  = [("Access-Control-Allow-Headers", "*"), ("Content-Type", "application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods","POST,GET,OPTIONS")] , 
                    apiGatewayResponseBody = b,
                    apiGatewayResponseIsBase64Encoded = False
                  }