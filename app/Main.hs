{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- the following two directives are absolutely needed for runLambda 
-- to compile and work 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Exception
import Prelude

import ExternalInterfaces.ApplicationAssembly (createApp, loadConfig, servApp)
import InterfaceAdapters.Config
import ExternalInterfaces.ServantShim
import Network.Wai
  ( Application
  )
import AWSLambda.Events.APIGateway

catchAllHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e

main :: IO ()
main = handle catchAllHandler $ redirectmain

redirectmain :: IO ()
redirectmain = handle catchAllHandler $ do
      app <- servApp -- | ApplicationAssembly 
      apiGatewayMain $ makeHandler app -- | makeHandler is in ServantShim
