{-# LANGUAGE TypeInType, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module InterfaceAdapters.Preferences where

import Data.Maybe
import Polysemy
    ( Sem, Member, Embed, embed, runM, interpret, makeSem )
import Data.Function             ((&))
import InterfaceAdapters.Parameters.KVS ( getKvs, insertKvs )
import InterfaceAdapters.Parameters.KVSAWSSSMParmStore
    ( runKvsAsAWSSSMParmStore )
import InterfaceAdapters.Utils.Helper
import Data.Text (Text)
import Data.Text as T
    ( Text, pack, words, isInfixOf, null, toLower )
import Data.Text.Encoding as T (encodeUtf8)
import InterfaceAdapters.Parameters.Types
    ( ParameterName, ParameterValue )
import Amazonka.SSM (ParameterTier(ParameterTier_Advanced))
import Data.Data (cast)
import Amazonka (FromJSON, ToJSON, eitherDecode)
import Amazonka.Prelude (Generic)
import qualified Data.ByteString.Lazy as TL
import Data.Aeson (eitherDecodeStrict)

data Preferences = Preferences {
  userdata :: Agdata
, usersize :: Datasize
, usertimespan :: Timespan
} deriving (Show, Generic, Eq)

instance FromJSON Preferences
instance ToJSON Preferences

data Agdata = Weather | WaterLevels | WeatherWaterLevels | Monsoon | All deriving (Show, Eq, Generic, FromJSON, ToJSON)
data Datasize = Mini | Standard | Detailed deriving (Show, Eq, Generic, FromJSON, ToJSON)
data Timespan = RightNow | Alerts | NearForecast | LongRange deriving (Show, Eq, Generic, FromJSON, ToJSON)

modalUser :: Text
modalUser = ""

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

setPreferences :: Text -> Text -> IO ()
setPreferences = runSetParm

getPreferences :: Text -> IO Preferences
getPreferences user_id = do
  x <- runGetParm user_id
  let y = eitherDecodeStrict $ T.encodeUtf8 x :: Either String Preferences
  case y of
    Left err -> do
        logMessage "No Preferences for user id. Getting one for modal user"
        return Preferences {userdata = Weather, usersize = Mini, usertimespan = NearForecast}
    Right prefs -> return prefs

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

parsePrefs :: T.Text -> T.Text -> T.Text
parsePrefs uuid prefsText
  | somePrefs = do
    let allPrefsvalid = Prelude.map (`T.isInfixOf` allPossiblePrefs) listPrefs
    let allPrefsOK = and allPrefsvalid
    if allPrefsOK
      then do
        createPrefsJSON prefsText
      else allPossiblePrefs
  | otherwise = allPossiblePrefs
  where
    somePrefs = not $ T.null prefsText
    listPrefs = T.words $ T.toLower prefsText
    allPossiblePrefs = T.toLower "Weather | WaterLevels | WeatherWaterLevels | Monsoon | All ||| Mini | Standard | Detailed ||| RightNow | Alerts | NearForecast | LongRange" :: T.Text
    textPrefsJSON = createPrefsJSON prefsText

-- {"userdata":"Weather", "usersize": "Mini","usertimespan":"NearForecast"}
createPrefsJSON :: T.Text -> T.Text
createPrefsJSON plainText = do
  let udata = "{\"userdata\": " :: T.Text
  let usize = "\"usersize\": " :: T.Text
  let utimespan = "\"usertimespan\": " :: T.Text
  -- have to insert real parse of plaintext below
  let udataPref = "\"Weather\"" :: T.Text
  let usizePref = "\"Mini\"" :: T.Text
  let utimespanPref = "\"RightNow\"" :: T.Text
  let totalPref = udata <> udataPref <> "," <> usize <> usizePref <> "," <> utimespan <> utimespanPref <> "}"
  totalPref
