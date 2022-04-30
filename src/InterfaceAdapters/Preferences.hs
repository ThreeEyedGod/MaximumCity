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
import Data.Text as T
import InterfaceAdapters.Parameters.Types
import Amazonka.SSM (ParameterTier(ParameterTier_Advanced))
import Data.Data (cast)

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

textToParameterType :: Text -> Maybe ParameterName
textToParameterType t  = cast t :: Maybe ParameterName

getPreferences :: Text -> IO Preferences
getPreferences user_id = do 
  let v = "{\"userdata\":\"Weather\", \"usersize\": \"Mini\",\"usertimespan\":\"NearForecast\"}" :: ParameterValue
  logMessage $ T.unpack user_id  
  let puser_id  = textToParameterType user_id
  case puser_id of
    Nothing -> error "no user_id in incoming telegram"
    Just p  -> do 
      runSetParm p v
      logMessage "runSetParm done "
      x <- runGetParm p
      logMessage "After runGetParm "
      logMessage (show x)
  return Preferences {userdata = Weather, usersize = Mini, usertimespan = NearForecast}

data MyPreferences m a where
  ReadPrefs :: String -> MyPreferences m Preferences
  SetPrefs :: String -> String -> MyPreferences m ()

makeSem ''MyPreferences

runprefsToIO :: Member (Embed IO) r => Sem (MyPreferences ': r) a -> Sem r a
runprefsToIO = interpret (\(ReadPrefs userId) -> embed (getPreferences $ T.pack userId))

runGetPrefs :: String -> IO Preferences
runGetPrefs userid = do 
  readPrefs userid
  & runprefsToIO
  & runM

