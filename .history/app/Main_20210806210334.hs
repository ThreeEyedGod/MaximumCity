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

-- | get token for using telegram
-- | if it is not there, because something trigerred the lambda
-- | it might still be salvageable so call runLambda 
main :: IO ()
main =  do
      res <- getTelegramSettings 
      case res of 
            Left msg -> runLambda (pure ()) (run Nothing)
            Right tk -> runLambda (pure ()) (run (Just tk))
      where
            run :: Maybe TC -> RunCallback APIGatewayHandlerType context
            run tc opts = do -- | ignore the 'context' part of opts for now
                  inComingEvent <- case (eitherDecode (eventObject opts)) of  
                        Left _  -> error "Fail No Event" 
                        Right inComingEvent -> pure inComingEvent
                  
                  result <- processApiGatewayRequest tc inComingEvent (contextObject opts)
                  return . pure $ result

