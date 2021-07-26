
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
  dly_summary :: String,
  dly_icon :: String,
  dly_data :: [DarkSkyDataPointDailyDetails]
} deriving (Show, Generic)
instance FromJSON DarkSkyDataPointDaily where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 4
  }
keywordFieldLabelModifier "__summary" = "summary"
keywordFieldLabelModifier "__data" = "data"
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
keywordFieldLabelModifier "_neareststation" = "nearest-station"
keywordFieldLabelModifier id = id

data DarkSkyDataPointDailyDetails = DarkSkyDataPointDailyDetails {
  dd_time :: Int,
  dd_icon :: String,
  dd_summary :: String, 
  dd_unriseTime :: Int,
  dd_sunsetTime :: Int, 
  dd_moonPhase :: Float,
  dd_precipIntensityMax :: Float, 
  dd_precipIntensityMaxTime :: Int, 
  dd_precipProbability :: Float, 
  dd_precipAccumulation :: Float, 
  dd_precipType :: String, 
  dd_temperatureHigh :: Float, 
  dd_temperatureHighTime :: Int, 
  dd_temperatureLow :: Float, 
  dd_temperatureLowTime :: Int, 
  dd_apparentTemperatureHigh :: Float, 
  dd_apparentTemperatureHighTime :: Int, 
  dd_apparentTemperatureLow :: Float, 
  dd_apparentTemperatureLowTime :: Int, 
  dd_dewPoint :: Float, 
  dd_humidity :: Float, 
  dd_pressure :: Float, 
  dd_windSpeed :: Float, 
  dd_windGust :: Float, 
  dd_windBearing :: Float, 
  dd_cloudCover :: Float, 
  dd_uvIndex :: Float, 
  dd_uvIndexTime :: Int, 
  dd_visibility :: Float, 
  dd_temperatureMin :: Float, 
  dd_temperatureMinTime :: Int, 
  dd_temperatureMax :: Float, 
  dd_temperatureMaxTime :: Int, 
  dd_apparentTemperatureMin :: Float, 
  dd_apparentTemperatureMinTime :: Int, 
  dd_apparentTemperatureMax :: Float, 
  dd_apparentTemperatureMaxTime :: Int
} deriving (Show, Generic)
instance FromJSON DarkSkyDataPointDailyDetails where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 3
  }
data DarkSkyFlags = DarkSkyFlags {
  flg_sources :: [String],
  flg_neareststation :: Int, 
  flg_units :: String
} deriving (Show, Generic)
instance FromJSON DarkSkyFlags where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 4
  }

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
instance FromJSON DarkSkyAlert
--instance FromJSON DarkSkyFlags
--instance FromJSON DarkSkyDataPointDaily
--instance FromJSON DarkSkyDataPointDailyDetails
