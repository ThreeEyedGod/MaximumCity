{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Aws.Lambda
import Data.Text
import Data.Maybe
import Prelude
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.ByteString (ByteString)
Network.HTTP.Types.Header
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
            run :: ByteString -> RunCallback APIGatewayHandlerType context
            run b opts = do
                  let responseHeaders :: ResponseHeaders = [("Access-Control-Allow-Headers", "*")]
                  case decodeUtf8' b of 
                        Right responseBodyText -> return . pure . APIGatewayResult $  200 responseHeaders responseBodyText
                        Left err -> error "bad!"
