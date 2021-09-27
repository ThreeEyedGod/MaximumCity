
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
import Data.Either

type LatLong = Text
type Key = String
defaultKey = "NoKey" :: Key

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

_goGetDarkSkyJson :: LatLong -> Either String Key -> IO (Either String DarkSky)
_goGetDarkSkyJson ll kee 
  | "Fail:" `isPrefixOf` ll = return $ Left $ unpack $ _returnStdFail "getWeatherForTown" "getLatLongforThis"
  | isLeft kee = return $ Left $ unpack $ _returnStdFail "getWeatherForTown" "getPirateWeatherSettings"
  | otherwise  =  do 
        x <- (eitherDecode <$> (getJSON (theURL (fromRight defaultKey kee)) ll)) :: IO (Either String DarkSky)
        case x of 
          Left e -> return $ Left $ unpack $ _returnStdFail "getWeatherForTown" "eitherDecode DarkSky"
          Right stuff -> return $ Right $ stuff 

_extractWeather :: Either String DarkSky -> IO Text
_extractWeather dS 
  | isLeft dS = return $  _returnStdFail "getWeatherForTown" "eitherDecode DarkSky"
  | isRight dS = do 
      let Right d = dS
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

_getWeatherForTown :: String -> IO Text
_getWeatherForTown town = do 
    a <- getLatLongforThis town
    b <- getPirateWeatherSettings
    d <- _goGetDarkSkyJson a b
    _extractWeather d 

{- getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  if "Fail:" `isPrefixOf` c
    then do 
      pure $ _returnStdFail "getWeatherForTown" "getLatLongforThis"
    else do 
      k <- getPirateWeatherSettings -- set this in AWS Env. Variables
      case k of
       Left errMsgString -> pure $ _returnStdFail "getWeatherForTown" "getPirateWeatherSettings"
       Right key ->  do
          d <- (eitherDecode <$> (getJSON (theURL key) c)) :: IO (Either String DarkSky)
          case d of
            Left e ->  pure $ _returnStdFail "getWeatherForTown" "eitherDecode" 
            Right stuff -> _extractWeather stuff
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
