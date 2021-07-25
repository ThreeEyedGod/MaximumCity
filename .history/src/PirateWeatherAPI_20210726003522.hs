
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

keywordFieldLabelModifier "_data" = "data"
keywordFieldLabelModifier "_summary" = "summary"

keywordFieldLabelModifier id = id
instance FromJSON DarkSkyDataPointDaily where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }

data DarkSkyDataPointDailyDetails = DarkSkyDataPointDailyDetails {
  time :: Float,
  icon :: String,
  sunriseTime :: Float,
  sunsetTime :: Float, 
  moonPhase :: Float 
  precipIntensityMax :: Float, 
  precipIntensityMaxTime :: Float, 
  precipProbability :: Float, 
  precipAccumulation :: Float, 
  precipType :: String, 
  temperatureHigh :: Float, 
  temperatureHighTime :: Float, 
  temperatureLow :: Float, 
  temperatureLowTime :: Float, 
  apparentTemperatureHigh :: Float, 
  apparentTemperatureHighTime :: Float, 
  apparentTemperatureLow :: Float, 
  apparentTemperatureLowTime :: Float, 
  dewPoint :: 74.33635598059537, 
  "humidity": 0.8704836277392909, 
  "pressure": 987.8381899572393, 
  "windSpeed": 14.36, 
  "windGust": 22.63, 
  "windBearing": 254.88059268512313, 
  "cloudCover": 1.0, 
  "uvIndex": 4.079084037061086, 
  "uvIndexTime": 1627210800, 
  "visibility": 9.44, 
  "temperatureMin": 77.66176330799077, 
  "temperatureMinTime": 1627246800, 
  "temperatureMax": 79.91098731300103, 
  "temperatureMaxTime": 1627210800, 
  "apparentTemperatureMin": 96.85225191896474, 
  "apparentTemperatureMinTime": 1627246800, 
  "apparentTemperatureMax": 99.80683339468393, 
  "apparentTemperatureMaxTime": 1627210800
} deriving (Show, Generic)

data DarkSkyFlags = DarkSkyFlags {
  sources :: [String],
  -- can't have a hyphen nearest-station :: Int, 
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
instance FromJSON DarkSkyFlags
instance ToJSON DarkSkyFlags
instance ToJSON DarkSkyDataPointDaily
instance FromJSON DarkSkyDataPointDailyDetails
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
