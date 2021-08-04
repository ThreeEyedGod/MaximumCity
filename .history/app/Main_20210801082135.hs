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
import Data.Typeable (Typeable)

import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )


main :: IO ()

ll a. m a -> IO a) ->
  HandlersM handlerType m context request response error () ->
  IO ()
runLambda :: forall context handlerType. IO context -> RunCallback handlerType context -> IO ()
type RunCallback (handlerType :: HandlerType) context = LambdaOptions context -> 
  IO (Either (LambdaError handlerType) (LambdaResult handlerType))
--}


main = do   
  runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) 
          id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler 

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))
gatewayHandler request context = 
      return . pure . mkApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] $ mempty
    