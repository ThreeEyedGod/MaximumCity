{-# LANGUAGE TypeInType, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module InterfaceAdapters.Preferences where

import Data.Maybe
import Polysemy
import Data.Function             ((&))
import InterfaceAdapters.Parameters.KVS
import InterfaceAdapters.Parameters.KVSAWSSSMParmStore
import InterfaceAdapters.Utils.Helper
import Data.Text (Text)
import InterfaceAdapters.Parameters.Types
import Amazonka.SSM (ParameterTier(ParameterTier_Advanced))

data Preferences = Preferences {
  userdata :: Agdata
, usersize :: Datasize
, usertimespan :: Timespan
}

data Agdata = Weather | WaterLevels | WeatherWaterLevels | Monsoon | All deriving (Show, Eq)
data Datasize = Mini | Standard | Detailed deriving (Show, Eq)
data Timespan = RightNow | Alerts | NearForecast | LongRange deriving (Show, Eq)

runGetParm :: ParameterName -> IO Text
runGetParm key = do 
  getKvs key
  & runKvsAsAWSSSMParmStore
  & runM

runSetParm :: ParameterName -> ParameterValue -> IO ()
runSetParm key val = do 
  insertKvs key val 
  & runKvsAsAWSSSMParmStore
  & runM

getPreferences :: IO Preferences
getPreferences = do 
  let p1 = "Malcolm" :: ParameterName 
  let v1 = "{\"userdata\":\"Weather\", \"usersize\": \"Mini\",\"usertimespan\":\"NearForecast\"}" :: ParameterValue
  runSetParm p1 v1
  logMessage "runSetParm done "
  x <- runGetParm p1
  logMessage "After runGetParm "
  logMessage (show x)
  return Preferences {userdata = Weather, usersize = Mini, usertimespan = NearForecast}

data MyPreferences m a where
  ReadPrefs :: String -> MyPreferences m Preferences
  SetPrefs :: String -> String -> MyPreferences m ()

makeSem ''MyPreferences

runprefsToIO :: Member (Embed IO) r => Sem (MyPreferences ': r) a -> Sem r a
runprefsToIO = interpret (\(ReadPrefs userId) -> embed getPreferences)

runGetPrefs :: String -> IO Preferences
runGetPrefs userid = do 
  readPrefs userid
  & runprefsToIO
  & runM

