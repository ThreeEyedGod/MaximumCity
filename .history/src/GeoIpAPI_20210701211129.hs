
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module GeoIpAPI where
import System.Environment
import Data.Aeson as Q
-- Import Aeson's internals
import Data.Aeson.Internal
import Data.Aeson.Parser.Internal
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
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data Locdata = Locdata
  { latitude :: Float,
    longitude :: Float
  }
  deriving (Show, Generic)

instance FromJSON Locdata
instance ToJSON Locdata
data ForwardGeoData = ForwardGeoData {
  _data :: [Locdata]
} deriving (Show, Generic)

keywordFieldLabelModifier "_data" = "data"
keywordFieldLabelModifier id = id
instance FromJSON ForwardGeoData where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.drop 1
  }
instance ToJSON ForwardGeoData
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
-- Gets LatLong for an IP 
jsonGeoIpURL :: String
jsonGeoIpURL = "https://freegeoip.app/json/"
getGeoIpforThis ::  IO B.ByteString
getGeoIpforThis = simpleHttp jsonGeoIpURL `X.catch` statusExceptionHandler
jsonPositionStackURL :: String
jsonPositionStackURL = "http://api.positionstack.com/v1/forward?access_key="

-- Gets LatLong only for a place only in INDIA (IN)
getForwardGeoCodeforThis :: String -> IO B.ByteString
getForwardGeoCodeforThis town = do
  key <- getEnv "API_POSITIONSTACK_KEY" -- set this in AWS Env. Variables
  let urlCall = jsonPositionStackURL ++ key ++ "&limit=1&fields=results.latitude,results.longitude&country=IN&query=" ++ town
  simpleHttp urlCall  `X.catch` statusExceptionHandler
 
getLatLongforThis :: String -> IO Text
getLatLongforThis town = do 
  d <- (eitherDecode <$> getForwardGeoCodeforThis town) :: IO (Either String Locdata)
  case d of
    Left e ->  do 
                f <- (eitherDecode <$> getGeoIpforThis) :: IO (Either String GeoIp)
                case f of 
                  Left err -> return $ "Mystery Place" <> pack err
                  Right geoipstuff_backup -> return $ Data.ByteString.Char8.pack (show (latitude (geoipstuff_backup :: GeoIp))  ++ "," ++ show (longitude (geoipstuff_backup :: GeoIp)))
    Right geoipstuff -> return $ Data.ByteString.Char8.pack (show latitude (geoipstuff) ++ "," ++ show  longitude (geoi))
