
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PirateWeatherAPI where
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
import Helper

data DarkSky = DarkSky {
    latitude::Float,
    longitude::Float,
    timezone::String,
    currently:: DarkSkyDataPoint,
    --minutely::DarkSkyDataPoint,
    --hourly:: DarkSkyDataPoint,
    --daily :: DarkSkyDataPointDaily,
    alerts :: [DarkSkyAlert],
    flags :: DarkSkyFlags,
    offset :: Float
} deriving (Show, Generic)

data DarkSkyDataPointDaily = DarkSkyDataPointDaily {
  _summary :: String,
  icon :: String,
  _data :: [DarkSkyDataPointDailyDetails]
} deriving (Show, Generic)



keywordFieldLabelModifier "_precipProbability" =  "precipProbability"
keywordFieldLabelModifier "_precipType" = "precipType" 
keywordFieldLabelModifier "_dewPoint" = "dewPoint"
keywordFieldLabelModifier "_humidity" = "humidity" 
keywordFieldLabelModifier "_windSpeed" = "windSpeed"
keywordFieldLabelModifier "_windGust" = "windGust"
keywordFieldLabelModifier "_windBearing" = "windBearing"
keywordFieldLabelModifier "_cloudCover"  = "cloudCover" 
keywordFieldLabelModifier "_uvIndex" = "uvIndex"
keywordFieldLabelModifier "_visibility" = "visibility"
keywordFieldLabelModifier "_data" = "data"
keywordFieldLabelModifier "_summary" = "summary"
keywordFieldLabelModifier "_neareststation" = "nearest-station"
keywordFieldLabelModifier id = id

instance FromJSON DarkSkyDataPointDailyDetails where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }

instance FromJSON DarkSkyDataPointDaily where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }
instance FromJSON DarkSkyFlags where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }

data DarkSkyDataPointDailyDetails = DarkSkyDataPointDailyDetails {
  time :: Int,
  icon :: String,
  sunriseTime :: Float,
  sunsetTime :: Float, 
  moonPhase :: Float,
  precipIntensityMax :: Float, 
  precipIntensityMaxTime :: Float, 
  _precipProbability :: Float, 
  precipAccumulation :: Float, 
  _precipType :: String, 
  temperatureHigh :: Float, 
  temperatureHighTime :: Float, 
  temperatureLow :: Float, 
  temperatureLowTime :: Float, 
  apparentTemperatureHigh :: Float, 
  apparentTemperatureHighTime :: Float, 
  apparentTemperatureLow :: Float, 
  apparentTemperatureLowTime :: Float, 
  _dewPoint :: Float, 
  _humidity :: Float, 
  pressure :: Float, 
  _windSpeed :: Float, 
  _windGust :: Float, 
  _windBearing :: Float, 
  _cloudCover :: Float, 
  _uvIndex :: Float, 
  uvIndexTime :: Float, 
  _visibility :: Float, 
  temperatureMin :: Float, 
  temperatureMinTime :: Float, 
  temperatureMax :: Float, 
  temperatureMaxTime :: Float, 
  apparentTemperatureMin :: Float, 
  apparentTemperatureMinTime :: Float, 
  apparentTemperatureMax :: Float, 
  apparentTemperatureMaxTime :: Float
} deriving (Show, Generic)

data DarkSkyFlags = DarkSkyFlags {
  sources :: [String],
  _neareststation :: Int, 
  units :: String
} deriving (Show, Generic)

data DarkSkyAlert = DarkSkyAlert {
            title :: String,
            time :: Float,
            expires :: Float,
            description :: String,
            uri :: String,
            severity :: String,
            regions ::[String]
} deriving (Show, Generic)

data DarkSkyDataPoint = DarkSkyDataPoint {
            time::Float,
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

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  d <- (eitherDecode <$> (getJSON "https://api.pirateweather.net/forecast/C5CbUhSoxE35OEbZwFRYY3dZ9X4Mw5nB3PMtqBT3/" c)) :: IO (Either String DarkSky)
  case d of
    Left e ->  return $ "Fail " <> pack e
    Right stuff -> do
        let rightNowWeather = parseNowWeather (currently stuff) 
        case maybeHead $ (alerts stuff) of
            Just firstAlert -> do
              --firstAlert <- (Prelude.take 1 someAlerts)!!0
              return $ Data.ByteString.Char8.pack $ (Prelude.unlines [ --unlines sticks in newline after each element
                (catSS "ALERT! " $ title $ firstAlert ),
                (catSS "ALERT! What: " $ description $ firstAlert ),
                " "] ++ rightNowWeather)
            Nothing -> do 
              --case maybeHead $ (PirateWeatherAPI._data (daily stuff)) of 
                --Just dailyForecast -> return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
                  --    (catSF "Tomorrow! " $ sunriseTime $ dailyForecast ), " "]
                --Nothing -> 
                  return $ Data.ByteString.Char8.pack $ rightNowWeather
  
parseNowWeather :: DarkSkyDataPoint -> String
parseNowWeather dsdp = Prelude.unlines [ --unlines sticks in newline after each element
                    (catSF "at Celsius " $ fahrenheitToCelsius (temperature dsdp)),
                    (catSS "" (summary dsdp)),
                    (catSF  "Chance of rain % " (100 * precipProbability dsdp)),
                    (catSF  "Intensity of rain mm/hour " (precipIntensity dsdp)),
                    (catSS  "Type of rain " (precipType dsdp)), 
                    (catSF  "Dew Point Celsius " (dewPoint dsdp)), 
                    (catSF  "Humdity  % " (100 * (humidity dsdp))), 
                    (catSF  "Windspeed at m/s " (windSpeed dsdp)), 
                    (catSF  "Wind Gust m/s " (windGust dsdp)), 
                    (catSF  "Wind Bearing " (windBearing dsdp)), 
                    (catSF  "Cloudcover " (cloudCover dsdp)), 
                    (catSF  "UV Index " (uvIndex dsdp)), 
                    (catSF  "Visibility Km " (visibility dsdp)), 
                    (catSF  "Ozone " (ozone dsdp)), 
                    (catSI  "Nearest Storm Distance Km " (nearestStormDistance dsdp)), 
                    (catSI  "Nearest Storm Bearing " (nearestStormBearing dsdp)), 
                    " "]
