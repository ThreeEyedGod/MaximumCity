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
import Network.HTTP.Types.Header
import qualified Network.HTTP.Types as H
import Telegram

main :: IO ()
main =  do
      res <- getTelegramSettings
      case res of 
            Left msg -> runLambda (pure ()) (run msg undefined)
            Right tk -> runLambda (pure ()) (run "" tk)
      where
            run :: String -> TC -> RunCallback APIGatewayHandlerType context
            run s tc opts = do
                  let responseHeaders :: ResponseHeaders = [("Access-Control-Allow-Headers", "*")]
                  let responsestatusCode = H.mkStatus 200  $ mempty
                  let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody body
                  return . pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText

{--
    run s tc opts
      | not (Prelude.null s) = do -- no telegram token. Route it to bad handler
          result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
      | otherwise = do -- OK case regular handler will deal with it
          result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
          --}