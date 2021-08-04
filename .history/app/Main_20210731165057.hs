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

runLambdaHaskellRuntime :: RuntimeContext handlerType m context request response error => DispatcherOptions -> 
  IO context -> (forall a. m a -> IO a) -> HandlersM handlerType m context request response error () -> IO ()
runLambdaHaskellRuntime ::
  RuntimeContext handlerType m context request response error =>
  DispatcherOptions ->
  IO context ->
  (forall a. m a -> IO a) ->
  HandlersM handlerType m context request response error () ->
  IO ()
main = runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) 
          id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler request context = do
    pure $ Right $ ApiGatewayResponse 200 [("","")] "OK" False