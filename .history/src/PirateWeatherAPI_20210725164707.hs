
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
    --daily::DarkSkyDataPoint,
    alerts :: [DarkSkyAlert],
    flags :: DarkSkyFlags,
    offset :: Float
} deriving (Show, Generic)

data DarkSkyFlags = DarkSkyFlags {
  sources
  nearest 
}

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

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  d <- (eitherDecode <$> (getJSON "https://api.pirateweather.net/forecast/C5CbUhSoxE35OEbZwFRYY3dZ9X4Mw5nB3PMtqBT3/" c)) :: IO (Either String DarkSky)
  case d of
    Left e ->  return $ "Fail " <> pack e
    Right stuff -> do
        case maybeHead $ (alerts stuff) of
            Just firstAlert -> do
              --firstAlert <- (Prelude.take 1 someAlerts)!!0
              return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
                (catSS "ALERT! " $ title $ firstAlert ),
                (catSS "ALERT! What: " $ description $ firstAlert ),
                " "]
            Nothing -> do 
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
