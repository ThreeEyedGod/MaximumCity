
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
import GHC.Float as SF 
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

  

data ForwardGeoCode = ForwardGeoCode {
            latitude :: Float,
            longitude :: Float
} deriving (Show, Generic) 

instance FromJSON ForwardGeoCode
instance ToJSON ForwardGeoCode
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
  d <- (eitherDecode <$> getForwardGeoCodeforThis town) :: IO (Either String ForwardGeoCode)
  case d of
    Left e ->  do 
                f <- (eitherDecode <$> getGeoIpforThis town) :: IO (Either String GeoIp)
                case f of 
                  Left err -> return $ "Mystery Place" <> pack err
                  Right geoipstuff_backup -> return $ Data.ByteString.Char8.pack (show $ latitude (geoipstuff_backup :: GeoIp)  ++ "," ++ show $ longitude (geoipstuff_backup :: GeoIp))
    Right geoipstuff -> return $ Data.ByteString.Char8.pack (showFloat $ latitude (geoipstuff :: GeoIp) ++ "," ++ SF.showFloat  $ longitude (geoipstuff :: GeoIp))
