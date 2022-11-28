{-# LANGUAGE DuplicateRecordFields #-}

module InterfaceAdapters.Weather.PirateWeatherAPI where
import Data.Aeson (eitherDecode)
import Data.Text ( unpack, pack, isPrefixOf, Text )
import Data.Maybe ( Maybe(..), fromMaybe, isJust )
import Data.Monoid ( (<>) )
import Control.Applicative ( Applicative(pure), (<$>) )
import Control.Monad ( Monad((>>=), return) )
import GHC.Generics ()
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import Data.Either ( fromRight, isLeft, isRight )
import Data.Functor ( (<&>) )

import InterfaceAdapters.IP.GeoIpAPI ()
import InterfaceAdapters.Utils.JSONHelper ( getJSON )
import InterfaceAdapters.Utils.Helper
    ( _returnStdFail,
      catSF,
      catSI,
      catSS,
      fahrenheitToCelsius,
      getKey,
      maybeHead )
import InterfaceAdapters.Weather.PirateWeatherHeaders
    ( DarkSky(currently, daily),
      DarkSkyDataPoint(precipIntensity, precipType, dewPoint, windGust,
                       windBearing, cloudCover, uvIndex, visibility, ozone,
                       nearestStormDistance, nearestStormBearing, temperature, summary,
                       precipProbability, humidity, windSpeed),
      DarkSkyDataPointDaily(dly_data),
      DarkSkyDataPointDailyDetails(dd_uvIndex, dd_summary,
                                   dd_precipIntensityMax, dd_temperatureHigh, dd_temperatureLow) )
import InterfaceAdapters.Preferences ()
import InterfaceAdapters.IP.GeoLatLong (getLatLongforThis)
type LatLong = Text
type Key = String
defaultKey :: Key
defaultKey = "NoKey" :: Key

getPirateWeatherSettings :: IO (Either String String)
getPirateWeatherSettings = do
  tk <- getKey "PIRATE_WEATHER_TOKEN"
  case tk of
    Left msg -> pure $ Left "Pirate Weather Token error"
    Right token -> return $ Right token

jsonPirateWeatherURL :: String
jsonPirateWeatherURL = "https://api.pirateweather.net/forecast/"

theURL :: String -> String
theURL q = jsonPirateWeatherURL ++ q ++ "/"

getLatLongPirateKey:: String -> IO (LatLong , Either String Key)
getLatLongPirateKey town = getLatLongforThis town >>= (\a ->  getPirateWeatherSettings >>=  (\b -> pure ( a:: LatLong , b)))

townDarkSky :: String -> IO (Either String DarkSky)
townDarkSky town = getLatLongPirateKey town >>= getDarkSkyjson

getDarkSkyjson :: (LatLong , Either String Key) -> IO (Either String DarkSky)
getDarkSkyjson (ll , kee)
  | "Fail:" `isPrefixOf` ll = return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing Lat Long"
  | isLeft kee = return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing Pirate Key"
  | otherwise  =  do
        let t = pack (unpack ll ++ "?exclude=minutely,hourly,alerts")
        x <- (eitherDecode <$> getJSON (theURL (fromRight defaultKey kee)) t) :: IO (Either String DarkSky) -- Http Call to PIrate Net !
        case x of
          Left e -> return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing DarkSky json"
          Right stuff -> return $ Right stuff

-- Entry functions
_getWeatherForTown :: String -> IO Text
_getWeatherForTown town = getLatLongPirateKey town >>= getDarkSkyjson >>= (pure . _extractWeatherN)

weatherCurrentForecast :: String -> IO Text
weatherCurrentForecast town = townDarkSky town Data.Functor.<&> currentweatherForecast

weatherCurrentForecastMini :: String -> IO Text
weatherCurrentForecastMini town = townDarkSky town Data.Functor.<&> currentweatherForecastMini
-- Finish Entry Functions

currentweatherForecast :: Either String DarkSky -> Text
currentweatherForecast (Left _)     = "Missing DarkSky"
currentweatherForecast dS@(Right d) = fromMaybe "Missing DarkSky " (weatherCurrent dS) <> fromMaybe "No Forecast available " (weatherForecastN (dly_data (daily d)))

currentweatherForecastMini :: Either String DarkSky -> Text
currentweatherForecastMini (Left _)      = "Missing DarkSky"
currentweatherForecastMini dS@(Right d)  = fromMaybe "Missing DarkSky " (weatherCurrentMini dS) <> fromMaybe "No Forecast available " (weatherForecastMiniN (dly_data (daily d)))

currentWeather :: Either String DarkSky -> Text
currentWeather dS  = fromMaybe "Missing DarkSky " $ weatherCurrent dS

onlyWeatherCurrent :: String -> IO Text
onlyWeatherCurrent town  = townDarkSky town Data.Functor.<&> currentWeather

_extractWeatherN :: Either String DarkSky -> Text
_extractWeatherN (Left _) = "Missing DarkSky"
_extractWeatherN dS@(Right d) = fromMaybe "Missing DarkSky " (weatherCurrent dS) <> fromMaybe "Missing Forecast " (weatherForecastN (dly_data (daily d)))

-- Process an error string or Darksky to extract either weather, alerts or forecast 
weatherCurrent:: Either String DarkSky -> Maybe Text
weatherCurrent (Right d) = Just $ Data.ByteString.Char8.pack $ parseNowWeather (currently d)
weatherCurrent (Left _) = Nothing

weatherForecastN :: [DarkSkyDataPointDailyDetails]  -> Maybe Text
weatherForecastN [] = Nothing
weatherForecastN d  = Just $ Data.ByteString.Char8.pack (getAllDaysForecast d)

weatherCurrentMini :: Either String DarkSky -> Maybe Text
weatherCurrentMini (Right d) = Just $ Data.ByteString.Char8.pack $ parseNowWeatherMini (currently d)
weatherCurrentMini (Left _)  = Nothing

weatherForecastMiniN :: [DarkSkyDataPointDailyDetails] -> Maybe Text
weatherForecastMiniN [] = Nothing
weatherForecastMiniN d  = Just $ Data.ByteString.Char8.pack (getAllDaysForecastMini d)

-- | parse DarkSkyJSOn to extract out text for current weather, alerts and forecasts
parseNowWeather :: DarkSkyDataPoint -> String
parseNowWeather dsdp = Prelude.unlines [ --unlines sticks in newline after each element
                    catSF "Current temperature Celsius " $ fahrenheitToCelsius (temperature dsdp),
                    catSS "Conditions " (summary dsdp),
                    catSF  "Chance of rain % " (100 * precipProbability dsdp),
                    catSF  "Intensity of rain in mm/hour " (precipIntensity dsdp),
                    catSS  "Type of rain " (precipType dsdp),
                    catSF  "Dew Point Celsius " (dewPoint dsdp),
                    catSF  "Humidity  % " (100 * humidity dsdp),
                    catSF  "Windspeed at m/s " (windSpeed dsdp),
                    catSF  "Wind Gust m/s " (windGust dsdp),
                    catSF  "Wind Bearing " (windBearing dsdp),
                    catSF  "Cloudcover " (cloudCover dsdp),
                    catSF  "UV Index " (uvIndex dsdp),
                    catSF  "Visibility in Km " (visibility dsdp),
                    catSF  "Ozone " (ozone dsdp),
                    catSI  "Nearest Storm Distance Km " (nearestStormDistance dsdp),
                    catSI  "Nearest Storm Bearing " (nearestStormBearing dsdp),
                    "  "]


getAllDaysForecast :: [DarkSkyDataPointDailyDetails] -> String
getAllDaysForecast [] = []
getAllDaysForecast (x : xs) =
          Prelude.unlines [   catSI "Forecasts available for days: " (Prelude.length xs + 1),
                              catSS "Day Summary : " $ dd_summary x,
                              catSF "Max Rain mm " $ dd_precipIntensityMax x,
                              catSF "Max Temp Celsius " $ fahrenheitToCelsius (dd_temperatureHigh x),
                              catSF "Min Temp Celsius " $ fahrenheitToCelsius (dd_temperatureLow x),
                              catSF "UV Index " $ dd_uvIndex x,
                              "  "]
                              ++ getAllDaysForecast xs

parseNowWeatherMini :: DarkSkyDataPoint -> String
parseNowWeatherMini dsdp = Prelude.unlines [ --unlines sticks in newline after each element
                    catSF "Current Temperature Celsius " $ fahrenheitToCelsius (temperature dsdp),
                    catSS "Conditions " (summary dsdp),
                    catSF  "Chance of rain % " (100 * precipProbability dsdp),
                    catSF  "Humidity % " (100 * humidity dsdp),
                    catSF  "Windspeed at m/s " (windSpeed dsdp),
                    "  "]


getAllDaysForecastMini :: [DarkSkyDataPointDailyDetails] -> String
getAllDaysForecastMini [] = []
getAllDaysForecastMini (x : xs) =
          Prelude.unlines [
                              catSS "Tomorrow : " $ dd_summary x,
                              catSF "Max Rain mm " $ dd_precipIntensityMax x,
                              catSF "Max Temp Celsius " $ fahrenheitToCelsius (dd_temperatureHigh x),
                              catSF "Min Temp Celsius " $ fahrenheitToCelsius (dd_temperatureLow x),
                              "  "]


{- getAllalerts :: [DarkSkyAlert] -> String
getAllalerts [] = []
getAllalerts (x : xs) = 
          Prelude.unlines [   (catSS "ALERT! " $ alrt_title $ x ),
                              (catSS "Heads Up: " $ alrt_description $ x ),
                              "  "] 
                              ++ getAllalerts xs

 -}

{- currentweatherAlertsForecast :: Either String DarkSky -> Text
currentweatherAlertsForecast dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Alerts issued " $ weatherAlerts dS) <> (fromMaybe "No Forecast available " $ weatherForecast dS)

currentweatherAlerts :: Either String DarkSky -> Text
currentweatherAlerts dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Alerts issued " $ weatherAlerts dS)
 -}
{- currentAlertsForecast :: Either String DarkSky -> Text
currentAlertsForecast dS  = (fromMaybe "No Alerts issued " $ weatherAlerts dS) <> (fromMaybe "No Forecast available " $ weatherForecast dS)
 -}

{- weatherCurrentAlerts :: String -> IO Text
weatherCurrentAlerts town  = townDarkSky town >>= (\x -> pure $ currentweatherAlerts x)
 -}

{- weatherAlertsForecast :: String -> IO Text
weatherAlertsForecast town  = townDarkSky town >>= (\x -> pure $ currentAlertsForecast x)
 -}

{- weatherAlerts :: Either String DarkSky -> Maybe Text
weatherAlerts dS
  | (isRight dS) && (isJust . maybeHead $ (alerts d)) = Just $ Data.ByteString.Char8.pack $ getAllalerts (alerts d)
  | otherwise = Nothing
  where
    Right d = dS
 -}
