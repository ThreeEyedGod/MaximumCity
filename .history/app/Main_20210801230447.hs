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
      runLambda (pure ()) (run "ok")
      where
            run :: Text -> RunCallback APIGatewayHandlerType context
            run body opts = do
                  let responseHeaders :: ResponseHeaders = [("Access-Control-Allow-Headers", "*")]
                  let responsestatusCode = H.mkStatus 200  $ mempty
                  let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody body
                  return . pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
