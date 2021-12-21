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
import ExternalInterfaces.ServantShim (makeHandler)
import Network.Wai
  ( Application
  )
import AWSLambda.Events.APIGateway


main :: IO ()
main = handle catchAllHandler $ redirectmain

catchAllHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e

redirectmain :: IO ()
redirectmain = handle catchAllHandler $ do
      app <- servApp -- | in ApplicationAssembly 
      apiGatewayMain $ makeHandler app -- | makeHandler is in ServantShim
