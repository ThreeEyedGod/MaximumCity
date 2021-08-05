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
            run s tc opts = do -- | ignore the 'context' part of opts for now
            
            {--
                  case (eitherDecode (eventObject opts)) of  
                        Left _  -> error "Fail " -- | No "Event" in Telegram hand off
                        Right inComingEvent -> do
                              result <- processApiGatewayRequest s tc inComingEvent (contextObject opts)
                              return . pure $ result

            --}

