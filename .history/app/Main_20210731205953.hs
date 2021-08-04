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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.IO.Handle.FD (stderr)
import GHC.IO.Handle.Text (hPutStr)
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
  runLambda (pure()) (run defaultDispatcherOptions mToIO $ fmap snd . flip runStateT HM.empty . runHandlersM $ do addAPIGatewayHandler "api-gateway" gatewayHandler )
  
  {--runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) 
          id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler 
--}
gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler request context = do
      let result = APIGatewayResult (ApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] "OK" False)
      pure $ Right $ result
    