{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- the following two directives are absolutely needed for runLambda 
-- to compile and work 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Exception
import Aws.Lambda
import Data.Text
import Data.Maybe
import Prelude
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.ByteString (ByteString)
import Network.HTTP.Types.Header
import qualified Network.HTTP.Types as H
import InterfaceAdapters.Telegram.Telegram -- eventually to be removed
import Data.Aeson

import ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import InterfaceAdapters.Config
import ExternalInterfaces.ServantShim
import Network.Wai
  ( Application
  )
import Data.Aeson
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway

catchAllHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e

main :: IO ()
{- main =  handle catchAllHandler $  do
      res <- getTelegramSettings 
      case res of 
            Left msg -> runLambda (pure ()) (run Nothing)
            Right tk -> runLambda (pure ()) (run (Just tk))
      where
            run :: Maybe TC -> RunCallback APIGatewayHandlerType context
            run tc opts = do -- | ignore the 'context' part of opts for now
                  inComingEvent <- case (eitherDecode (eventObject opts)) of  
                        Left _ -> fail "Fail:main | No incoming Event"
                        Right inComingEvent -> pure inComingEvent
                  
                  result <- processApiGatewayRequest tc inComingEvent (contextObject opts)
                  return . pure $ result
 -}
main = handle catchAllHandler $ redirectmain

servApp :: IO Application 
servApp = do 
      c <- loadConfig
      pure $ createApp c 

redirectmain :: IO ()
redirectmain = handle catchAllHandler $ do
      app <- servApp
      apiGatewayMain $ makeHandler app
