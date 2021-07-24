
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
    currently:: DarkSkyDataPoint
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

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  c <- getLatLongforThis town
  d <- (eitherDecode <$> (getJSON "https://api.pirateweather.net/forecast/C5CbUhSoxE35OEbZwFRYY3dZ9X4Mw5nB3PMtqBT3/" c)) :: IO (Either String DarkSky)
  case d of
    Left e ->  return $ "Fail " <> pack e
    Right stuff ->
        return $ Data.ByteString.Char8.pack $ Prelude.unlines [ --unlines sticks in newline after each element
          (catSF "at Celsius " $ fahrenheitToCelsius (temperature (currently stuff))),
          (catSS "" (summary (currently stuff))),
          (catSF  "Chance of rain % " (100 * precipProbability (currently stuff))),
          (catSF  "Intensity of rain " (precipIntensity (currently stuff))),
          (catSS  "Type of rain " (precipType (currently stuff))), 
          (catSF  "Dew Point currently " (dewPoint (currently stuff))), 
          (catSF  "Humdity now " (humidity (currently stuff))), 
          (catSF  "Windspeed at " (windSpeed (currently stuff))), 
          (catSF  "Wind Gust " (windGust (currently stuff))), 
          (catSF  "Wind Bearing " (windBearing (currently stuff))), 
          (catSF  "Cloudcover " (cloudCover (currently stuff))), 
          (catSF  "UV Index " (windGust (currently stuff))), 
          (catSF  "Visibility " (windBearing (currently stuff))), 
          (catSF  "Cloudcover " (cloudCover (currently stuff))), 

          " "]


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