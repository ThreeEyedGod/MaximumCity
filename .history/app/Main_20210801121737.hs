{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Aws.Lambda
import Data.Text
import Prelude

runLambdaHaskellRuntime :: RuntimeContext handlerType m context request response error => 
DispatcherOptions -> 
IO context -> 
(forall a. m a -> IO a) 
-> HandlersM handlerType m context request response error () 
-> IO ()

runLambdaHaskellRuntime 
defaultDispatcherOptions 
(pure ()) 
id $ 
do addAPIGatewayHandler "api-gateway" gatewayHandler 

runLambda :: forall context handlerType. IO context -> RunCallback handlerType context -> IO ()


main :: IO ()
main =  do
      runLambda 

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))
gatewayHandler request context = 
      return . pure . mkApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] $ mempty
    