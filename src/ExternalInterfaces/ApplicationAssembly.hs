{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-@ LIQUID "--skip-module" @-}

module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                           (Input, runInputConst)
import           Polysemy.Trace                           (Trace, traceToIO, ignoreTrace, traceToStdout)
import           Servant.Server (Application, ServerT, Handler(..), errBody, err412, hoistServer, serve)
import           Data.Aeson.Types (ToJSON, FromJSON)

import           InterfaceAdapters.Config
import           InterfaceAdapters.AgricultureRestService
import           InterfaceAdapters.Weather.WWITelegramPirate
import           InterfaceAdapters.Weather.WWIWebPirate
import           InterfaceAdapters.Parameters.KVSAWSSSMParmStore
import           UseCases.WWI
import           UseCases.AgricultureUseCase


{--
  @LambdaFiring
  1. LoadConfig and then 'Select' the right backendfrontend runWWI___
  2. runWWI_ then sets it so that GetWeatherTown is 'interpreted' to Weather.getAgInfo
  2a. the structure of interpretation is in WWI

  @DataFlowIn
  1. Servant sucks in the actual API (POST/GET/whatever) in AgriRestServices and 
  1a. Depending on the http-call (Post or Get or etc) weatherTownTelegram or weatherTownWeb is called
  2. which then makes the call to the mapped runWWI_function in (in WWITelegramPirate, WWIWebPirate, other) and processes the frontend specific JSON or Text
  3. which then makes the call to AgriUsecase - weatherTown
  3. which then calls WWI:GetWeatherTown
  4. See @LambdaFiring#2!
--}

servApp :: IO Application
servApp = do createApp <$> loadConfig

-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> Application
createApp config = serve agricultureAPI (liftServer config)

liftServer :: Config -> ServerT AgricultureAPI Handler
liftServer config = hoistServer agricultureAPI (interpretServer config) agricultureServer
  where
    interpretServer conf sem  =  sem
      & selectWWICombination conf
      & runInputConst conf
      & runError @WeatherStatusError
      & selectTraceVerbosity conf
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . fmap handleErrors
    handleErrors (Left (WeatherStatusNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

-- | port and verbose are unused
selectWWICombination :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r)
                 => Config -> Sem (WWI : r) a -> Sem r a
selectWWICombination Config {port = 8080, backend = PirateWeather, frontend = Telegram, geoend = OpenCage, gateway = AWSAPIRest,  verbose = True} = runWWITelegramPirate
selectWWICombination Config {port = 8080, backend = PirateWeather, frontend = Web, geoend = OpenCage, gateway = AWSAPIRest,  verbose = True} = runWWIWebPirate

-- | if the config flag verbose is set to True, trace to Console, else ignore all trace messages
selectTraceVerbosity :: (Member (Embed IO) r) => Config -> (Sem (Trace : r) a -> Sem r a)
selectTraceVerbosity config =
  if verbose config
    then traceToStdout
    else ignoreTrace

