{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Aws.Lambda
import Data.Text
import Prelude

{--
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

runLambda :: forall context handlerType. IO context -> 
RunCallback handlerType context 
-> IO ()

type RunCallback (handlerType :: HandlerType) context = 
LambdaOptions context -> 
IO (Either (LambdaError handlerType) (LambdaResult handlerType))
--}

main :: IO ()
main =  do
      runLambda (pure ()) (run "ok")
      where
            --run :: String -> LambdaOptions context -> IO (Either APIGatewayLambdaError APIGatewayResult) 
            run :: String -> RunCallback APIGatewayHandlerType context
            run s opts = do
                  return . pure . APIGatewayResultmkApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] $ mempty
