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
import InterfaceAdapters.Utils.Helper ( logMessage )
import Data.Text (Text)
import Data.Text as T
    ( Text, pack, words, isInfixOf, null, toLower )
import Data.Text.Encoding as T (encodeUtf8)
import Data.Text.Manipulate as DTM
import Data.Sort as DS
import Data.Ord as O
import InterfaceAdapters.Parameters.Types
    ( ParameterName, ParameterValue )
import Amazonka.SSM (ParameterTier(ParameterTier_Advanced))
import Data.Data (cast)
import Amazonka (FromJSON, ToJSON, eitherDecode)
import Amazonka.Prelude (Generic)
import qualified Data.ByteString.Lazy as TL
import Data.Aeson (eitherDecodeStrict)
import qualified GHC.IO.Encoding as T

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
    listPrefs = DTM.splitWords $ T.toLower prefsText
    allPossiblePrefs = T.toLower "Weather | WaterLevels | WeatherWaterLevels | Monsoon | All ||| Mini | Standard | Detailed ||| RightNow | Alerts | NearForecast | LongRange" :: T.Text
    textPrefsJSON = createPrefsJSON prefsText

-- {"userdata":"Weather", "usersize": "Mini","usertimespan":"NearForecast"}
createPrefsJSON :: T.Text -> T.Text
createPrefsJSON plainText = do
  let udata = "{\"userdata\": " :: T.Text
  let usize = "\"usersize\": " :: T.Text
  let utimespan = "\"usertimespan\": " :: T.Text
  let listPrefs = DTM.splitWords $ T.toLower plainText
  let sortedList = prefsSort listPrefs
  let udataPref =  head sortedList
  let usizePref = head $ tail sortedList
  let utimespanPref = last sortedList
  --let udataPref = "\"Weather\"" :: T.Text
  --let usizePref = "\"Mini\"" :: T.Text
  --let utimespanPref = "\"RightNow\"" :: T.Text
  let totalPref = udata <> "\"" <> udataPref <> "\"" <>  "," <> usize <> "\"" <> usizePref <> "\"" <>  "," <> utimespan <> "\"" <> utimespanPref <> "\"" <>  "}"
  totalPref

sortByPrefs :: T.Text -> T.Text -> Ordering
sortByPrefs x y
  | x `T.isInfixOf` T.toLower "Weather | WaterLevels | WeatherWaterLevels | Monsoon | All" && y `T.isInfixOf` T.toLower "Mini | Standard | Detailed"  = LT
  | x `T.isInfixOf` T.toLower "Mini | Standard | Detailed" && y `T.isInfixOf` T.toLower "RightNow | Alerts | NearForecast | LongRange"= LT
  | x `T.isInfixOf` T.toLower "Mini | Standard | Detailed" && y `T.isInfixOf` T.toLower "Weather | WaterLevels | WeatherWaterLevels | Monsoon | All" = GT
  | x `T.isInfixOf` T.toLower "RightNow | Alerts | NearForecast | LongRange" = GT
  | otherwise = EQ

prefsSort :: [T.Text] -> [T.Text]
prefsSort xs = map replaceBy $ DS.sortBy sortByPrefs xs

replaceBy :: T.Text -> T.Text 
replaceBy x
  | x == "weather" = "Weather" :: T.Text
  | x == "waterlevels" = "WaterLevels" :: T.Text
  | x == "weatherwaterlevels" = "WeatherWaterLevels" :: T.Text
  | x == "monsoon" = "Monsoon" :: T.Text
  | x == "all" = "All" :: T.Text
  | x == "mini" = "Mini" :: T.Text
  | x == "standard" = "Standard" :: T.Text
  | x == "detailede" = "Detailed" :: T.Text
  | x == "rightnow" = "RightNow" :: T.Text
  | x == "alerts" = "Alerts" :: T.Text
  | x == "nearforecast" = "NearForecast" :: T.Text
  | x == "longrange" = "LongRange" :: T.Text
  | otherwise = "Hmm"
 where
    allPossiblePrefs = "Weather | WaterLevels | WeatherWaterLevels | Monsoon | All ||| Mini | Standard | Detailed ||| RightNow | Alerts | NearForecast | LongRange" :: T.Text
 