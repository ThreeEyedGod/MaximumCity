
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module GeoIpAPI where
import Data.Aeson as Q
import Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import GHC.Float as SF 
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import Helper
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE

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

getPositionStackSettings :: IO (Either String String)
getPositionStackSettings = do
  tk <- getKey "API_POSITIONSTACK_KEY"
  case tk of
    Left _ -> pure $ Left $ "PositionStack Token error"
    Right token -> pure $ Right $ token

-- Gets LatLong only for places only in INDIA (IN)
getForwardGeoCodefor :: String -> IO B.ByteString
getForwardGeoCodefor town = do
   k <- getPositionStackSettings -- set this in AWS Env. Variables
   case k of
     Left errMsgString -> 
          return $ TLE.encodeUtf8 $ TL.pack errMsgString -- make String ByteString
     Right key ->  do
          let urlCall = jsonPositionStackURL ++ key ++ "&limit=1&fields=results.latitude,results.longitude&country=IN&query=" ++ town
          simpleHttp urlCall `X.catch` statusExceptionHandler
 
getLatLongforThis :: String -> IO T.Text
getLatLongforThis town = do
    d <- (eitherDecode <$> getForwardGeoCodefor town) :: IO (Either String ForwardGeoData)
    case d of
      Left e -> do
        f <- (eitherDecode <$> getGeoIpforThis) :: IO (Either String GeoIp)
        case f of
          Left err -> return $ "Fail: " 
          Right geoipstuff_backup -> return $ Data.ByteString.Char8.pack (show (latitude (geoipstuff_backup :: GeoIp)) ++ "," ++ show (longitude (geoipstuff_backup :: GeoIp)))
      Right geoipstuff ->
        return $
          Data.ByteString.Char8.pack $ removeNonNumbers $
            (show (Prelude.head (_data (geoipstuff :: ForwardGeoData))))
