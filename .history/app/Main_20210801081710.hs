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

{--
runLambdaHaskellRuntime :: RuntimeContext handlerType m context request response error => DispatcherOptions -> 
  IO context -> (forall a. m a -> IO a) -> HandlersM handlerType m context request response error () -> IO ()
runLambdaHaskellRuntime ::
  RuntimeContext handlerType m context request response error =>
  DispatcherOptions ->
  IO context ->
  (forall a. m a -> IO a) ->
  HandlersM handlerType m context request response error () ->
  IO ()
runLambda :: forall context handlerType. IO context -> RunCallback handlerType context -> IO ()
type RunCallback (handlerType :: HandlerType) context = LambdaOptions context -> 
  IO (Either (LambdaError handlerType) (LambdaResult handlerType))
--}

main = do 
  type MyApiGatewayHandler = ApiGatewayRequest Text -> Context Application -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))
  type MyGenericHandler = Value -> Context Application -> IO (Either Value Value)
  --runLambda (pure()) (run defaultDispatcherOptions mToIO $ fmap snd . flip runStateT HM.empty . runHandlersM $ do addAPIGatewayHandler "api-gateway" gatewayHandler )
  
  runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) 
          id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler 


gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))
gatewayHandler request context = 
      case fromJSON @(ApiGatewayRequest Text) request of
          Success apiGwRequest ->
            bimap toJSON toJSON <$> apiGatewayWaiHandler apiGwRequest context
          Error err -> error $ "Could not parse the request as a valid API Gateway request: " <> err
      let result = ApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] "OK" False
      return . pure . mkApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] $ mempty
      pure $ Right $ result
    