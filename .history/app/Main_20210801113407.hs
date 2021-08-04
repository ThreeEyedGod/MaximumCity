{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Aws.Lambda
import Data.Text
import Prelude ()

main :: IO ()
main =  
  runLambdaHaskellRuntime 
        defaultDispatcherOptions 
        (pure ()) 
        id $ do 
            addAPIGatewayHandler "api-gateway" gatewayHandler 

gatewayHandler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))
gatewayHandler request context = 
      return . pure . mkApiGatewayResponse 200 [("Access-Control-Allow-Headers", "*")] $ mempty
    