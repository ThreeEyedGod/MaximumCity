
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GeoIpAPI where
import System.Environment
import Data.Aeson as Q
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

data ForwardGeoCode = ForwardGeoCode {
   data: {
      results: [
         {
            latitude: Float,
            longitude: -77.036547,
            "label": "1600 Pennsylvania Avenue NW, Washington, DC, USA",
            "name": "1600 Pennsylvania Avenue NW",
            "type": "address",
            "number": "1600",
            "street": "Pennsylvania Avenue NW",
            "postal_code": "20500",
            "confidence": 1,
            "region": "District of Columbia",
            "region_code": "DC",
            "administrative_area": null,
            "neighbourhood": "White House Grounds",
            "country": "United States",
            "country_code": "US",
            "map_url": "http://map.positionstack.com/38.897675,-77.036547"
         }
      ]
   }
}

data GeoIp = GeoIp {ip::String,
                    country_code::String,
                    country_name::String,
                    region_code::String,
                    region_name::String,
                    city::String,
                    zip_code::String,
                    time_zone::String,
                    latitude::Float,
                    longitude::Float,
                    metro_code::Int} deriving (Show, Generic)

instance FromJSON GeoIp
instance ToJSON GeoIp

statusExceptionHandler ::  SomeException -> IO B.ByteString
statusExceptionHandler e = (putStrLn "Bad Error") >> (return B.empty)

jsonGeoIpURL :: String
jsonGeoIpURL = "https://freegeoip.app/json/"
getGeoIpforThis :: String -> IO B.ByteString
getGeoIpforThis town = simpleHttp jsonGeoIpURL `X.catch` statusExceptionHandler

jsonPositionStackURL :: String
jsonPositionStackURL = "https://api.positionstack.com/v1/forward?access_key="
getForwardGeoCodeforThis :: String -> IO B.ByteString
getForwardGeoCodeforThis town = do
  key <- getEnv "API_POSITIONSTACK_KEY"
  let urlCall = jsonPositionStackURL ++ key ++ "&query=" ++ town
  simpleHttp urlCall  `X.catch` statusExceptionHandler

getLatLongforThis :: String -> IO Text
getLatLongforThis town = do 
  d <- (eitherDecode <$> getGeoIpforThis town) :: IO (Either String GeoIp)
  case d of
    Left e ->  do 
                f <- (eitherDecode <$> getForwardGeoCodeforThis town) :: IO (Either String )
                return $ "Mystery Place"
    Right geoipstuff -> return $ Data.ByteString.Char8.pack (show (latitude geoipstuff) ++ "," ++ show (longitude geoipstuff))
