{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                           (Input, runInputConst)
import           Polysemy.Trace                           (Trace, traceToIO, ignoreTrace, traceToStdout)
import           Servant.Server
import           Data.Aeson.Types (ToJSON, FromJSON)

import           InterfaceAdapters.Config
import           InterfaceAdapters.AgricultureRestService
import           InterfaceAdapters.Weather.WWITelegramPirate
import           InterfaceAdapters.Weather.WWIWebPirate
import           UseCases.WWI
import           UseCases.AgricultureUseCase


servApp :: IO Application 
servApp = do 
      c <- loadConfig
      pure $ createApp c 
      
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
                 => Config -> Sem (WWI PlaceName TheWeatherThere : r) a -> Sem r a
selectWWICombination Config {port = 8080, backend = PirateWeather, frontend = Telegram, gateway = AWSAPIRest, verbose = True} = runWWITelegramPirate
selectWWICombination Config {port = 8080, backend = PirateWeather, frontend = Web, gateway = AWSAPIRest, verbose = True} = runWWIWebPirate

-- | if the config flag verbose is set to True, trace to Console, else ignore all trace messages
selectTraceVerbosity :: (Member (Embed IO) r) => Config -> (Sem (Trace : r) a -> Sem r a)
selectTraceVerbosity config =
  if verbose config
    then traceToStdout
    else ignoreTrace

-- | load application config. In real life, this would load a config file or read commandline args.
-- | port, verbose are unused
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = PirateWeather, frontend = Telegram, gateway = AWSAPIRest, verbose = True}