
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module InterfaceAdapters.Weather.PirateWeatherAPI where
import Data.Aeson as Q
import Data.Text
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import GHC.Generics ()
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import Data.Either

import InterfaceAdapters.IP.GeoIpAPI
import InterfaceAdapters.Utils.JSONHelper
import InterfaceAdapters.Utils.Helper
import InterfaceAdapters.Weather.PirateWeatherHeaders
import InterfaceAdapters.Preferences

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

getLatLongPirateKey:: String -> IO (LatLong , Either String Key) 
getLatLongPirateKey town = getLatLongforThis town >>= (\a ->  ( getPirateWeatherSettings >>=  (\b -> pure $ ( a:: LatLong , b))))

townDarkSky :: String -> IO (Either String DarkSky)
townDarkSky town = getLatLongPirateKey town >>= (\z -> getDarkSkyjson z)

getDarkSkyjson :: (LatLong , Either String Key) -> IO (Either String DarkSky)
getDarkSkyjson (ll , kee) 
  | "Fail:" `isPrefixOf` ll = return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing Lat Long"
  | isLeft kee = return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing Pirate Key"
  | otherwise  =  do 
        let t = pack (unpack ll ++ "?exclude=minutely,hourly,daily,alerts")
        x <- (eitherDecode <$> (getJSON (theURL (fromRight defaultKey kee)) t)) :: IO (Either String DarkSky) -- | Http Call to PIrate Net !
        case x of 
          Left e -> return $ Left $ unpack $ _returnStdFail "getDarkSkyjson" "Missing DarkSky json"
          Right stuff -> return $ Right $ stuff 

{- currentweatherAlertsForecast :: Either String DarkSky -> Text
currentweatherAlertsForecast dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Alerts issued " $ weatherAlerts dS) <> (fromMaybe "No Forecast available " $ weatherForecast dS)

currentweatherAlerts :: Either String DarkSky -> Text
currentweatherAlerts dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Alerts issued " $ weatherAlerts dS)
 -}
currentweatherForecast :: Either String DarkSky -> Text
currentweatherForecast dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Forecast available " $ weatherForecast dS)

{- currentAlertsForecast :: Either String DarkSky -> Text
currentAlertsForecast dS  = (fromMaybe "No Alerts issued " $ weatherAlerts dS) <> (fromMaybe "No Forecast available " $ weatherForecast dS)
 -}
currentWeather :: Either String DarkSky -> Text
currentWeather dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) 

onlyWeatherCurrent :: String -> IO Text
onlyWeatherCurrent town  = townDarkSky town >>= (\x -> pure $ currentWeather x)

{- weatherCurrentAlerts :: String -> IO Text
weatherCurrentAlerts town  = townDarkSky town >>= (\x -> pure $ currentweatherAlerts x)
 -}
weatherCurrentForecast :: String -> IO Text
weatherCurrentForecast town  = townDarkSky town >>= (\x -> pure $ currentweatherForecast x)

{- weatherAlertsForecast :: String -> IO Text
weatherAlertsForecast town  = townDarkSky town >>= (\x -> pure $ currentAlertsForecast x)
 -}
_getWeatherForTown :: String -> IO Text
_getWeatherForTown town =  getLatLongPirateKey town >>= (\z -> getDarkSkyjson z) >>=  (\x -> pure $ _extractWeatherN x)

_extractWeatherN :: Either String DarkSky -> Text
-- _extractWeatherN dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "No Alerts " $ weatherAlerts dS) <> (fromMaybe "Missing Forecast " $ weatherForecast dS)
_extractWeatherN dS  = (fromMaybe "Missing DarkSky " $ weatherCurrent dS) <> (fromMaybe "Missing Forecast " $ weatherForecast dS)

{- _extractWeather :: Either String DarkSky -> IO Text
_extractWeather dS 
  | isLeft dS = pure $ _returnStdFail "getWeatherForTown" "eitherDecode DarkSky"
  | isRight dS = do 
      let Right d = dS
      let rightNowWeather = parseNowWeather (currently d) 
      case maybeHead $ (alerts d) of
          Just atLeastOneAlert -> pure $ Data.ByteString.Char8.pack $ rightNowWeather ++ getAllalerts (alerts d) 
          Nothing -> do 
            case maybeHead $ (dly_data (daily d)) of 
              Just dailyForecast -> pure $ Data.ByteString.Char8.pack $ rightNowWeather ++ 
                                      getAllDaysForecast (dly_data (daily d))
              Nothing -> pure $ Data.ByteString.Char8.pack $ rightNowWeather
 -}


-- | Process an error string or Darksky to extract either weather, alerts or forecast 
weatherCurrent:: Either String DarkSky -> Maybe Text
weatherCurrent dS 
  | isLeft dS =  Nothing -- | _returnStdFail "weatherCurrent" "Missing DarkSky"
  | isRight dS = Just $ Data.ByteString.Char8.pack $ parseNowWeather (currently d) 
  where Right d = dS

weatherForecast:: Either String DarkSky -> Maybe Text
weatherForecast dS 
  | (isRight dS) && (isJust . maybeHead $ (dly_data (daily d))) = Just $ Data.ByteString.Char8.pack $ getAllDaysForecast (dly_data (daily d))
  | otherwise = Nothing
  where 
    Right d  = dS 

{- weatherAlerts :: Either String DarkSky -> Maybe Text
weatherAlerts dS 
  | (isRight dS) && (isJust . maybeHead $ (alerts d)) = Just $ Data.ByteString.Char8.pack $ getAllalerts (alerts d) 
  | otherwise = Nothing 
  where 
    Right d = dS 
 -}
-- | parse DarkSkyJSOn to extract out text for current weather, alerts and forecasts
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

{- getAllalerts :: [DarkSkyAlert] -> String
getAllalerts [] = []
getAllalerts (x : xs) = 
          Prelude.unlines [   (catSS "ALERT! " $ alrt_title $ x ),
                              (catSS "Heads Up: " $ alrt_description $ x ),
                              "  "] 
                              ++ getAllalerts xs

 -}