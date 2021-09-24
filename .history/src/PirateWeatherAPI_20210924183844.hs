
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PirateWeatherAPI where
import Data.Aeson as Q
import Data.Text
import GeoIpAPI
import JSONHelper
import Control.Applicative
import Control.Monad
import GHC.Generics ()
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import Helper
import PirateWeatherHeaders

getPirateWeatherSettings :: IO (Either String String)
getPirateWeatherSettings = do
  tk <- getKey "PIRATE_WEATHER_TOKEN"
  case tk of
    Left msg -> pure $ Left $ "Pirate Weather Token error"
    Right token -> return $ Right $ token

jsonPirateWeatherURL :: String
jsonPirateWeatherURL = "https://api.pirateweather.net/forecast/"

theURL :: String -> String
theURL q = jsonPirateWeatherURL ++ q ++ "/"

_preProcessPirateWeather :: Text -> String -> IO DarkSky
_preProcessPirateWeather latlong key 
  | "Fail:" `isPrefixOf` latlong = return $ "Fail:getWeatherForTown | getLatLongforThis"

_extractWeather :: DarkSky -> IO Text
_extractWeather d = do
      let rightNowWeather = parseNowWeather (currently d) 
      case maybeHead $ (alerts d) of
          Just atLeastOneAlert -> do
                return $ Data.ByteString.Char8.pack $ rightNowWeather ++ getAllalerts (alerts d) 
          Nothing -> do 
            case maybeHead $ (dly_data (daily d)) of 
              Just dailyForecast -> do
                return $ Data.ByteString.Char8.pack $ rightNowWeather ++ 
                      getAllDaysForecast (dly_data (daily d))
              Nothing -> 
                return $ Data.ByteString.Char8.pack $ rightNowWeather

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  if "Fail:" `isPrefixOf` c
    then do 
      return $ "Fail:getWeatherForTown | getLatLongforThis"
    else do 
      k <- getPirateWeatherSettings -- set this in AWS Env. Variables
      case k of
       Left errMsgString -> return $ "Fail:getWeatherForTown | getPirateWeatherSettings "
       Right key ->  do
          d <- (eitherDecode <$> (getJSON (theURL key) c)) :: IO (Either String DarkSky)
          case d of
            Left e ->  return $ "Fail:getWeatherForTown | eitherDecode key " <> pack e
            Right stuff -> _extractWeather stuff
{-             Right stuff -> do
                let rightNowWeather = parseNowWeather (currently stuff) 
                case maybeHead $ (alerts stuff) of
                    Just atLeastOneAlert -> do
                          return $ Data.ByteString.Char8.pack $ rightNowWeather ++
                                getAllalerts (alerts stuff) 
                    Nothing -> do 
                      case maybeHead $ (dly_data (daily stuff)) of 
                        Just dailyForecast -> do
                          return $ Data.ByteString.Char8.pack $ rightNowWeather ++ 
                                getAllDaysForecast (dly_data (daily stuff))
                        Nothing -> 
                          return $ Data.ByteString.Char8.pack $ rightNowWeather
 -}


getAllDaysForecast :: [DarkSkyDataPointDailyDetails] -> String
getAllDaysForecast [] = []
getAllDaysForecast (x : xs) = 
          Prelude.unlines [   (catSI "Forecasts available for days: " $ ((Prelude.length xs) + 1)),
                              (catSS "Tomorrow : " $ dd_summary $ x ),
                              (catSF "Max Rain mm " $ dd_precipIntensityMax $ x ),
                              (catSF "Max Temp Celsius " $ fahrenheitToCelsius (dd_temperatureHigh $ x) ),
                              (catSF "Min Temp Celsius " $ fahrenheitToCelsius (dd_temperatureLow $ x) ),
                              (catSF "UV Index " $ dd_uvIndex $ x),
                              "  "] 
                              ++ getAllDaysForecast xs

getAllalerts :: [DarkSkyAlert] -> String
getAllalerts [] = []
getAllalerts (x : xs) = 
          Prelude.unlines [   (catSS "ALERT! " $ alrt_title $ x ),
                              (catSS "Heads Up: " $ alrt_description $ x ),
                              "  "] 
                              ++ getAllalerts xs

parseNowWeather :: DarkSkyDataPoint -> String
parseNowWeather dsdp = Prelude.unlines [ --unlines sticks in newline after each element
                    (catSF "at Celsius " $ fahrenheitToCelsius (temperature dsdp)),
                    (catSS "It is " (summary dsdp)),
                    (catSF  "Chance of rain % " (100 * precipProbability dsdp)),
                    (catSF  "Intensity of rain in mm/hour " (precipIntensity dsdp)),
                    (catSS  "Type of rain " (precipType dsdp)), 
                    (catSF  "Dew Point Celsius " (dewPoint dsdp)), 
                    (catSF  "Humdity  % " (100 * (humidity dsdp))), 
                    (catSF  "Windspeed at m/s " (windSpeed dsdp)), 
                    (catSF  "Wind Gust m/s " (windGust dsdp)), 
                    (catSF  "Wind Bearing " (windBearing dsdp)), 
                    (catSF  "Cloudcover " (cloudCover dsdp)), 
                    (catSF  "UV Index " (uvIndex dsdp)), 
                    (catSF  "Visibility in Km " (visibility dsdp)), 
                    (catSF  "Ozone " (ozone dsdp)), 
                    (catSI  "Nearest Storm Distance Km " (nearestStormDistance dsdp)), 
                    (catSI  "Nearest Storm Bearing " (nearestStormBearing dsdp)), 
                    "  "]
