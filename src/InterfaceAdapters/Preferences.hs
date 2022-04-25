{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module InterfaceAdapters.Preferences where

import Data.Maybe
import Polysemy
import Data.Function             ((&))
import InterfaceAdapters.Parameters.KVS
import InterfaceAdapters.Parameters.KVSAWSSSMParmStore
import InterfaceAdapters.Utils.Helper

data Preferences = Preferences {
  userdata :: Agdata
, usersize :: Datasize
, usertimespan :: Timespan
}

data Agdata = Weather | WaterLevels | WeatherWaterLevels | Monsoon | All deriving (Show, Eq)
data Datasize = Mini | Standard | Detailed deriving (Show, Eq)
data Timespan = RightNow | Alerts | NearForecast | LongRange deriving (Show, Eq)

runGetParm :: String -> IO (Maybe String)
runGetParm key = do 
  getKvs key
  & runKvsAsAWSSSMParmStore
  & runM

runSetParm :: String -> String -> IO ()
runSetParm key val = do 
  insertKvs key val 
  & runKvsAsAWSSSMParmStore
  & runM

getPreferences :: IO Preferences
getPreferences = do 
  runSetParm "/AAA/BBB" "CCC"
  logMessage "runSetParm done "
  x <- runGetParm "/AAA/BBB"
  logMessage "After runGetParm "
  logMessage (fromMaybe "Nothing" x)
  return Preferences {userdata = Weather, usersize = Mini, usertimespan = NearForecast}

data MyPreferences m a where
  ReadPrefs :: String -> MyPreferences m Preferences

makeSem ''MyPreferences

runprefsToIO :: Member (Embed IO) r => Sem (MyPreferences ': r) a -> Sem r a
runprefsToIO = interpret (\(ReadPrefs userId) -> embed $ getPreferences)

runGetPrefs :: String -> IO Preferences
runGetPrefs userid = do 
  readPrefs userid
  & runprefsToIO
  & runM

