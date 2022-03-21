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

import ExternalInterfaces.ApplicationAssembly (createApp, servApp)
import InterfaceAdapters.Config
import ExternalInterfaces.ServantShim (makeHandler)
import Network.Wai
  ( Application
  )
import AWSLambda.Events.APIGateway

import           InterfaceAdapters.Utils.Helper
import           InterfaceAdapters.Parameters.AWSSSMParmStore (doGetParameter, doPutParameter, ParameterName (..), ParameterValue (..), ssmService)
import           InterfaceAdapters.Parameters.AWSViaHaskell
import Network.AWS.Auth
import Network.AWS.S3.Types
import           Language.Haskell.TH
import           Network.AWS (Service)
import           Control.Lens

main :: IO ()
main = handle catchAllHandler $ redirectmain

catchAllHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e

redirectmain :: IO ()
redirectmain = handle catchAllHandler $ do
      let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ FromProfile "MaximumCity-role-3x5dxzgh"
      ssmSession <- connect conf ssmService
      result1 <- doPutParameter (ParameterName "/AAA/BBB") (ParameterValue "CCC") ssmSession
      (value, version) <- doGetParameter (ParameterName "/AAA/BBB") ssmSession
      logMessage $ "Value: " <> show value

      app <- servApp -- | in ApplicationAssembly 
      apiGatewayMain $ makeHandler app -- | makeHandler is in ServantShim
