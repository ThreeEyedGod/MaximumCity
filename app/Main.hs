{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- the following two directives are absolutely needed for runLambda 
-- to compile and work 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Exception (handle, SomeException(..))
import Prelude
import AWSLambda.Events.APIGateway (apiGatewayMain)
import ExternalInterfaces.ApplicationAssembly (createApp, servApp)
import ExternalInterfaces.ServantShim (makeHandler)

import InterfaceAdapters.Config

main :: IO ()
main = handle catchAllHandler redirectmain

catchAllHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e

redirectmain :: IO ()
redirectmain = handle catchAllHandler $ do
      app <- servApp -- | in ApplicationAssembly 
      apiGatewayMain $ makeHandler app -- | makeHandler is in ServantShim
