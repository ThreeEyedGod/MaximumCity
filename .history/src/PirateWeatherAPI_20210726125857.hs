
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
import PirateWeatherHeaders

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  if "Fail " `isPrefixOf` c
    then do 
      return $ "Fail "
    else do 
      d <- (eitherDecode <$> (getJSON "https://api.pirateweather.net/forecast/C5CbUhSoxE35OEbZwFRYY3dZ9X4Mw5nB3PMtqBT3/" c)) :: IO (Either String DarkSky)
      case d of
        Left e ->  return $ "Fail " <> pack e
        Right stuff -> do
            let rightNowWeather = parseNowWeather (currently stuff) 
            case maybeHead $ (alerts stuff) of
                Just atLeastOneAlert -> do
                   return $ Data.ByteString.Char8.pack $ 
                            getAllalerts (alerts stuff) 
                            ++ rightNowWeather
                Nothing -> do 
                  case maybeHead $ (_data (daily stuff)) of 
                    Just dailyForecast -> do
                    return $ Data.ByteString.Char8.pack $ 
                            getAllDaysForecast (_data (daily stuff)) 
                            ++ rightNowWeather
                    Nothing -> 
                      return $ Data.ByteString.Char8.pack $ rightNowWeather

getAllDaysForecast [DarkSkyDataPointDailyDetails] -> String
getAllDaysForecast [] = []
getAllDaysForecast ( x : ) 


getAllalerts :: [DarkSkyAlert] -> String
getAllalerts [] = []
getAllalerts (x : xs) = 
          Prelude.unlines [   (catSS "ALERT! " $ title $ x ),
                              (catSS "Heads Up: " $ description $ x ),
                              " "] 
                              ++ getAllalerts xs

parseNowWeather :: DarkSkyDataPoint -> String
parseNowWeather empty = null
parseNowWeather dsdp = Prelude.unlines [ --unlines sticks in newline after each element
                    (catSF "at Celsius " $ fahrenheitToCelsius (temperature dsdp)),
                    (catSS "It is " (summary dsdp)),
                    (catSF  "Chance of rain % " (100 * precipProbability dsdp)),
                    (catSF  "Intensity of rain in mm/hour " (precipIntensity dsdp)),
                    (catSS  "Type of rain " (precipType dsdp)), 
                    (catSF  "Dew Point in Celsius " (dewPoint dsdp)), 
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
                    " "]
