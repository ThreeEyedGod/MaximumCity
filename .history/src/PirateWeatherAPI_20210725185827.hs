
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
    daily::DarkSkyDataPointDaily,
    alerts :: [DarkSkyAlert],
    flags :: DarkSkyFlags,
    offset :: Float
} deriving (Show, Generic)

data DarkSkyDataPointDaily = DarkSkyDataPointDaily {
  summary :: String,
  icon :: String
  -- can't use this word eh ? data :: [DarkSkyDataPointDailyDetails]
} deriving (Show, Generic)

data DarkSkyDataPointDailyDetails = DarkSkyDataPointDailyDetails {
  time :: Float,
  icon :: String,
  sunriseTime :: Float,
  sunsetTime :: Float, 
  moonPhase :: Float 
  -- to be added 
  -- "precipIntensityMax": 0.06912740484668026, "precipIntensityMaxTime": 1627210800, "precipProbability": 1.0, "precipAccumulation": 0.6217063293140745, "precipType": "rain", "temperatureHigh": 79.91098731300103, "temperatureHighTime": 1627210800, "temperatureLow": 77.66176330799077, "temperatureLowTime": 1627246800, "apparentTemperatureHigh": 99.80683339468393, "apparentTemperatureHighTime": 1627210800, "apparentTemperatureLow": 96.85225191896474, "apparentTemperatureLowTime": 1627246800, "dewPoint": 74.33635598059537, "humidity": 0.8704836277392909, "pressure": 987.8381899572393, "windSpeed": 14.36, "windGust": 22.63, "windBearing": 254.88059268512313, "cloudCover": 1.0, "uvIndex": 4.079084037061086, "uvIndexTime": 1627210800, "visibility": 9.44, "temperatureMin": 77.66176330799077, "temperatureMinTime": 1627246800, "temperatureMax": 79.91098731300103, "temperatureMaxTime": 1627210800, "apparentTemperatureMin": 96.85225191896474, "apparentTemperatureMinTime": 1627246800, "apparentTemperatureMax": 99.80683339468393, "apparentTemperatureMaxTime": 1627210800
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
instance FromJSON DarkSkyDataPointDaily
instance ToJSON DarkSkyDataPointDaily
instance FromJSON DarkSkyDataPointDailyDetails
instance ToJSON DarkSkyDataPointDailyDetails

parserightNowWeather :: DarkSky -> [String]
parserightNowWeather win = 


getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  d <- (eitherDecode <$> (getJSON "https://api.pirateweather.net/forecast/C5CbUhSoxE35OEbZwFRYY3dZ9X4Mw5nB3PMtqBT3/" c)) :: IO (Either String DarkSky)
  case d of
    Left e ->  return $ "Fail " <> pack e
    Right stuff -> do
        let rightNowWeather = parserightNowWeather stuff 
        case maybeHead $ (alerts stuff) of
            Just firstAlert -> do
              --firstAlert <- (Prelude.take 1 someAlerts)!!0
              return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
                (catSS "ALERT! " $ title $ firstAlert ),
                (catSS "ALERT! What: " $ description $ firstAlert ),
                " "]
            Nothing -> do 
              case maybeHead $ (daily stuff) of 
                Just dailyForecast -> return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
                (catSS "Tomorrow! " $ summary $ dailyForecast ), " "]
                Nothing -> 
                  return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
                    (catSF "at Celsius " $ fahrenheitToCelsius (temperature (currently stuff))),
                    (catSS "" (summary (currently stuff))),
                    (catSF  "Chance of rain % " (100 * precipProbability (currently stuff))),
                    (catSF  "Intensity of rain mm/hour " (precipIntensity (currently stuff))),
                    (catSS  "Type of rain " (precipType (currently stuff))), 
                    (catSF  "Dew Point Celsius " (dewPoint (currently stuff))), 
                    (catSF  "Humdity  % " (100 * (humidity (currently stuff)))), 
                    (catSF  "Windspeed at m/s " (windSpeed (currently stuff))), 
                    (catSF  "Wind Gust m/s " (windGust (currently stuff))), 
                    (catSF  "Wind Bearing " (windBearing (currently stuff))), 
                    (catSF  "Cloudcover " (cloudCover (currently stuff))), 
                    (catSF  "UV Index " (uvIndex (currently stuff))), 
                    (catSF  "Visibility Km " (visibility (currently stuff))), 
                    (catSF  "Ozone " (ozone (currently stuff))), 
                    (catSI  "Nearest Storm Distance Km " (nearestStormDistance (currently stuff))), 
                    (catSI  "Nearest Storm Bearing " (nearestStormBearing (currently stuff))), 
                    " "]
