
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
    Left e ->  return $ "Fail"
    Right stuff ->
        return $ Data.ByteString.Char8.pack $ [
          (catSF "Current Tgemperature" (temperature (currently stuff)))
      ++ " F" ++ catSS ("" summary (currently stuff))
      ++  (catSF  "Chance of rain " (100 * precipProbability (currently stuff)))
       ++ 
      "Intensity of rain " ++ show (precipIntensity (currently stuff)) ++ "\n" ++
      "Type of rain " ++ show (precipType (currently stuff))) ++ "\n"
      
catSS :: String -> String -> String
catSS r s  = r ++ s 

catSF :: String -> Float -> String
catSF r f = r ++ show f

catSI :: String -> Int -> String
catSI r i = r ++ show i