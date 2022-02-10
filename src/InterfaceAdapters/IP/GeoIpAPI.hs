
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module InterfaceAdapters.IP.GeoIpAPI where
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
import InterfaceAdapters.Utils.Helper
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE

data OpenCageLicenseData = OpenCageLicenseData {
    name :: String
  , url  :: String
} deriving (Show, Generic, FromJSON)
data OpenCageRateData = OpenCageRateData {
    limit :: Int
  , remaining :: Int
  , reset     :: Int
} deriving (Show, Generic, FromJSON)

data OpenCageBoundsData  = OpenCageBoundsData {
            northeast :: OpenCageLocdata,
            southwest :: OpenCageLocdata
         } deriving (Show, Generic, FromJSON)
{- data OpenCageComponentsData = OpenCageComponentsData {
            ISO_3166-1_alpha-2 :: String,
            ISO_3166-1_alpha-3 :: String,
            _category :: String,
            _type :: String,
            city :: String,
            continent :: String,
            country :: String,
            country_code :: String,
            municipality :: String,
            state :: String,
            state_code :: String,
            state_district :: String
         } deriving (Show, Generic)
 -}
data OpenCageResultData = OpenCageResultData {
         bounds :: OpenCageBoundsData,
         -- components :: OpenCageComponentsData,
         confidence :: Int,
         formatted :: String,
         geometry :: OpenCageLocdata
} deriving (Show, Generic, FromJSON)

--instance FromJSON OpenCageForwardGeoData
--instance ToJSON OpenCageForwardGeoData

data OpenCageStatus = OpenCageStatus {
      code :: Int
    , message :: String
   } deriving (Show, Generic, FromJSON)

data OpenCagestayInformed = OpenCagestayInformed {
      blog :: String
    , twitter :: String
   } deriving (Show, Generic, FromJSON)

data OpenCagetimestamp = OpenCagetimestamp {
      created_http :: String
    , created_unix :: Int
   } deriving (Show, Generic, FromJSON)

data OpenCageForwardGeoData = OpenCageForwardGeoData {
    documentation :: String
  , licenses      :: [OpenCageLicenseData]
  , rate          :: OpenCageRateData
  , results       :: [OpenCageResultData]
  , status        :: OpenCageStatus
  , stay_informed :: OpenCagestayInformed 
  , thanks        :: String
  , timestamp     :: OpenCagetimestamp 
  , total_results :: Int
} deriving (Show, Generic, FromJSON)

data OpenCageLocdata = OpenCageLocdata
  { lat :: Float
  , lng :: Float
  } deriving (Show, Generic, FromJSON)
--instance FromJSON OpenCageLocdata
--instance ToJSON OpenCageLocdata

data Locdata = Locdata
  { latitude :: Float,
    longitude :: Float
  } deriving (Show, Generic)

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
jsonOpenCageURL = "https://api.opencagedata.com/geocode/v1/json?key=8d21df2243834a4f9d5336725c5cc179&q="

getPositionStackSettings :: IO (Either String String)
getPositionStackSettings = do
  tk <- getKey "API_POSITIONSTACK_KEY"
  case tk of
    Left _ -> pure $ Left $ "Fail:getPositionStackSettings | PositionStack Token error"
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

-- Gets LatLong only for places only in INDIA (IN)
getOpenCageForwardGeoCodefor :: String -> IO B.ByteString
getOpenCageForwardGeoCodefor town = do
          let urlCall = jsonOpenCageURL ++ town ++ "&pretty=1&no_annotations=1&countrycode=in&limit=1"
          simpleHttp urlCall `X.catch` statusExceptionHandler

getLatLongforThis :: String -> IO T.Text
getLatLongforThis town = do
    d <- (eitherDecode <$> getForwardGeoCodefor town) :: IO (Either String ForwardGeoData)
    case d of
      Left e -> do
        -- f <- (eitherDecode <$> getGeoIpforThis) :: IO (Either String GeoIp)
        f <- (eitherDecode <$> getOpenCageForwardGeoCodefor town) :: IO (Either String OpenCageForwardGeoData)
        case f of
          Left err -> return $ "Fail:getLatLongforThis | eitherDecode getGeoIpforThis" 
          -- Right geoipstuff_backup -> return $ Data.ByteString.Char8.pack (show (latitude (geoipstuff_backup :: GeoIp)) ++ "," ++ show (longitude (geoipstuff_backup :: GeoIp)))
          Right geo_backup -> return $ Data.ByteString.Char8.pack (show (lat (geometry (Prelude.head (results (geo_backup :: OpenCageForwardGeoData))))) ++ "," ++ show (lng (geometry (Prelude.head (results (geo_backup :: OpenCageForwardGeoData))))))
      Right geoipstuff ->
        return $
          Data.ByteString.Char8.pack $ removeNonNumbers $
            (show (Prelude.head (_data (geoipstuff :: ForwardGeoData))))
