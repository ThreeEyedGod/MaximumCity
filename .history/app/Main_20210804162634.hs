{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- the following two directives are absolutely needed for runLambda 
-- to compile and work 
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
import HttpHeadersPathDefinitions
import Data.Aeson
import Lib

main :: IO ()
main =  do
      res <- getTelegramSettings
      case res of 
            Left msg -> runLambda (pure ()) (run (pack msg) undefined)
            Right tk -> runLambda (pure ()) (run "" tk)
      where
            run :: Text -> TC -> RunCallback APIGatewayHandlerType context
            run s tc opts
                  | not (Prelude.null (unpack s)) = do -- no telegram token. Route it to bad handler
                        case (decode (eventObject opts) of  
                              Left 
                        result <- badhandler (unpack s) (decode (eventObject opts)) (contextObject opts)
                        return . pure $ result
                  | otherwise = do -- OK case regular handler will deal with it
                        result <- handler tc (decode (eventObject opts)) (contextObject opts)
                        return . pure $ result
{-- 
                  let responseHeaders :: ResponseHeaders = [("Access-Control-Allow-Headers", "*")]
                  let responsestatusCode = H.mkStatus 200  $ mempty
                  let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody s
                  return . pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
--}
