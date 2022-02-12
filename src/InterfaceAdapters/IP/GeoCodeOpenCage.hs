
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module InterfaceAdapters.IP.GeoCodeOpenCage (getOpenCageForwardGeoCodefor, OpenCageForwardGeoData (..), OpenCageResultData(..), OpenCageLocdata (..)) where
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
import Data.Char as DC

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
            northeast :: OpenCageLocdata
        ,   southwest :: OpenCageLocdata
         } deriving (Show, Generic, FromJSON)

data OpenCageComponentsData = OpenCageComponentsData {
            iso_3166_1_alpha_2 :: String,
            iso_3166_1_alpha_3 :: String,
            _category :: String,
            _type :: String,
            city :: String,
            continent :: String,
            country :: String,
            country_code :: String,
            county :: String,
            postcode :: String,
            state :: String,
            state_code :: String,
            state_district :: String
         } deriving (Show, Generic)

hyphentoUnderscore_toLower :: Char -> Char 
hyphentoUnderscore_toLower x 
      | generalCategory x == DashPunctuation = '_'
      | otherwise         = DC.toLower x

instance FromJSON OpenCageComponentsData where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = Prelude.map hyphentoUnderscore_toLower
    --, constructorTagModifier = Prelude.map hyphentoUnderscore_toLower
    --, constructorTagModifier = camelTo2 '_'
  }
instance ToJSON OpenCageComponentsData where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = Prelude.map hyphentoUnderscore_toLower
    --, constructorTagModifier = Prelude.map hyphentoUnderscore_toLower
    --, constructorTagModifier = camelTo2 '_'
  }

data OpenCageResultData = OpenCageResultData {
         --  bounds :: OpenCageBoundsData
         --, components :: OpenCageComponentsData
         --, confidence :: Int,
         --, formatted :: String
             geometry :: OpenCageLocdata
} deriving (Show, Generic, FromJSON)

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
  --  documentation :: String
  --, licenses      :: [OpenCageLicenseData]
  --, rate          :: OpenCageRateData
      results       :: [OpenCageResultData]
  --, status        :: OpenCageStatus
  --, stay_informed :: OpenCagestayInformed 
  --, thanks        :: String
  --, timestamp     :: OpenCagetimestamp 
  --, total_results :: Int
} deriving (Show, Generic, FromJSON)

data OpenCageLocdata = OpenCageLocdata
  { lat :: Float
  , lng :: Float
  } deriving (Show, Generic, FromJSON)

statusExceptionHandler ::  SomeException -> IO B.ByteString
statusExceptionHandler e = (putStrLn "Bad Error") >> (return B.empty)
jsonOpenCageURL = "https://api.opencagedata.com/geocode/v1/json?key=8d21df2243834a4f9d5336725c5cc179&q="

getOpenCageSettings :: IO (Either String String)
getOpenCageSettings = key "API_OPENCAGE_KEY"

-- Gets LatLong only for places only in INDIA (IN)
getOpenCageForwardGeoCodefor :: String -> IO B.ByteString
getOpenCageForwardGeoCodefor town = do
          k <- getOpenCageSettings -- set this in AWS Env. Variables
          case k of 
            Left errMsgString ->   return $ TLE.encodeUtf8 $ TL.pack errMsgString -- make String ByteString
            Right key         ->   do
                  let urlCall = jsonOpenCageURL ++ town ++ "&pretty=1&no_annotations=1&countrycode=in&limit=1"
                  simpleHttp urlCall `X.catch` statusExceptionHandler
