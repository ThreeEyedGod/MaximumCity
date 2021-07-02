
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

getLatLongforThis :: String -> IO Text
getLatLongforThis town = do 
  d <- (eitherDecode <$> getGeoIpforThis town) :: IO (Either String GeoIp)
  case d of
    Left e ->  return $ "Mystery Place"
    Right geoipstuff -> return $ Data.ByteString.Char8.pack (show (latitude geoipstuff) ++ "," ++ show (longitude geoipstuff))
