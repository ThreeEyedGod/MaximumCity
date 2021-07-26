
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PirateWeatherHeaders where
import System.Environment
import Data.Aeson as Q
import Data.Text
import GeoIpAPI
import JSONHelper
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

data DarkSky = DarkSky {
    latitude::Float,
    longitude::Float,
    timezone::String,
    currently:: DarkSkyDataPoint,
    --minutely::DarkSkyDataPoint,
    --hourly:: DarkSkyDataPoint,
    daily :: DarkSkyDataPointDaily,
    alerts :: [DarkSkyAlert],
    --flags :: DarkSkyFlags, -- nearest station causing problems 
    offset :: Float
} deriving (Show, Generic)

data DarkSkyDataPointDaily = DarkSkyDataPointDaily {
  __summary :: String,
  icon :: String,
  __data :: [DarkSkyDataPointDailyDetails]
} deriving (Show, Generic)

keywordFieldLabelModifier "__data" = "data"
keywordFieldLabelModifier "__summary" = "summary"
keywordFieldLabelModifier "_neareststation" = "nearest-station"
keywordFieldLabelModifier id = id

instance FromJSON DarkSkyDataPointDailyDetails where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 3
  }

instance FromJSON DarkSkyDataPointDaily where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 2
  }
instance FromJSON DarkSkyFlags where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }
keywordFieldLabelModifier "___summary" = "summary"
keywordFieldLabelModifier "___precipProbability" =  "precipProbability"
keywordFieldLabelModifier "___precipType" = "precipType" 
keywordFieldLabelModifier "___dewPoint" = "dewPoint"
keywordFieldLabelModifier "___humidity" = "humidity" 
keywordFieldLabelModifier "___windSpeed" = "windSpeed"
keywordFieldLabelModifier "___windGust" = "windGust"
keywordFieldLabelModifier "___windBearing" = "windBearing"
keywordFieldLabelModifier "___cloudCover"  = "cloudCover" 
keywordFieldLabelModifier "___uvIndex" = "uvIndex"
keywordFieldLabelModifier "___visibility" = "visibility"
data DarkSkyDataPointDailyDetails = DarkSkyDataPointDailyDetails {
  time :: Int,
  icon :: String,
  ___summary :: String, 
  sunriseTime :: Int,
  sunsetTime :: Int, 
  moonPhase :: Float,
  precipIntensityMax :: Float, 
  precipIntensityMaxTime :: Int, 
  ____precipProbability :: Float, 
  precipAccumulation :: Float, 
  ____precipType :: String, 
  temperatureHigh :: Float, 
  temperatureHighTime :: Int, 
  temperatureLow :: Float, 
  temperatureLowTime :: Int, 
  apparentTemperatureHigh :: Float, 
  apparentTemperatureHighTime :: Int, 
  apparentTemperatureLow :: Float, 
  apparentTemperatureLowTime :: Int, 
  ___dewPoint :: Float, 
  ___humidity :: Float, 
  pressure :: Float, 
  ___windSpeed :: Float, 
  ___windGust :: Float, 
  ___windBearing :: Float, 
  ___cloudCover :: Float, 
  ___uvIndex :: Float, 
  uvIndexTime :: Int, 
  ___visibility :: Float, 
  temperatureMin :: Float, 
  temperatureMinTime :: Int, 
  temperatureMax :: Float, 
  temperatureMaxTime :: Int, 
  apparentTemperatureMin :: Float, 
  apparentTemperatureMinTime :: Int, 
  apparentTemperatureMax :: Float, 
  apparentTemperatureMaxTime :: Int
} deriving (Show, Generic)

data DarkSkyFlags = DarkSkyFlags {
  sources :: [String],
  _neareststation :: Int, 
  units :: String
} deriving (Show, Generic)

data DarkSkyAlert = DarkSkyAlert {
            title :: String,
            time :: Int,
            expires :: Float,
            description :: String,
            uri :: String,
            severity :: String,
            regions ::[String]
} deriving (Show, Generic)

data DarkSkyDataPoint = DarkSkyDataPoint {
            time::Int,
            summary:: String, 
            icon:: String, 
            nearestStormDistance:: Int, 
            nearestStormBearing:: Int, 
            precipIntensity:: Float, 
            precipProbability:: Float, 
            precipIntensityError:: Float, 
            precipType:: String, 
            temperature:: Float, 
            apparentTemperature:: Float, 
            dewPoint:: Float, 
            humidity:: Float, 
            pressure:: Float, 
            windSpeed:: Float, 
            windGust:: Float, 
            windBearing:: Float, 
            cloudCover:: Float, 
            uvIndex:: Float, 
            visibility:: Float, 
            ozone:: Float
} deriving (Show, Generic)

instance FromJSON DarkSky
instance FromJSON DarkSkyDataPoint
instance ToJSON DarkSky
instance ToJSON DarkSkyDataPoint
instance FromJSON DarkSkyAlert
instance ToJSON DarkSkyAlert
--instance FromJSON DarkSkyFlags
instance ToJSON DarkSkyFlags
instance ToJSON DarkSkyDataPointDaily
--instance FromJSON DarkSkyDataPointDaily
--instance FromJSON DarkSkyDataPointDailyDetails
instance ToJSON DarkSkyDataPointDailyDetails