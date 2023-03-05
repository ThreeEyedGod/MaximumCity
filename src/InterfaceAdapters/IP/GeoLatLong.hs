
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-@ LIQUID "--skip-module" @-}

module InterfaceAdapters.IP.GeoLatLong (getLatLongforThis) where
import Data.Aeson (eitherDecode)
import Data.Text as T
import Control.Applicative
import Control.Monad
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import InterfaceAdapters.Utils.Helper
import InterfaceAdapters.IP.GeoCodeOpenCage (getOpenCageForwardGeoCodefor, OpenCageForwardGeoData (..), OpenCageResultData(..), OpenCageLocdata(..))
import InterfaceAdapters.IP.GeoIpAPI (getPositionStackForwardGeoCodefor, ForwardGeoData (..))
import InterfaceAdapters.Utils.ShortCircuit (orM)

getLatLongforThis :: String -> IO T.Text
getLatLongforThis town = getLLData town >>= geoDataJSONToText

getLLData :: String ->  IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
getLLData s = orM (getPositionStackJSON s >>= makeNestedEitherPositionStack) (getOpenCageJSON s >>= makeNestedEitherOpenCage) -- lazy evaluate first failover to second

getPositionStackJSON :: String -> IO (Either String ForwardGeoData)
getPositionStackJSON s = eitherDecode <$> getPositionStackForwardGeoCodefor s

makeNestedEitherPositionStack :: Either String ForwardGeoData -> IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
makeNestedEitherPositionStack (Left x)  = pure $ Left x
makeNestedEitherPositionStack (Right y) = pure $ Right $ Left y 

getOpenCageJSON :: String -> IO (Either String OpenCageForwardGeoData)
getOpenCageJSON s = eitherDecode <$> getOpenCageForwardGeoCodefor s

makeNestedEitherOpenCage :: Either String OpenCageForwardGeoData -> IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
makeNestedEitherOpenCage (Left x)  = pure $ Left x
makeNestedEitherOpenCage (Right y) = pure $ Right $ Right y 

geoDataJSONToText :: Either String (Either ForwardGeoData OpenCageForwardGeoData) -> IO T.Text
geoDataJSONToText (Left f)          = return $ T.pack f
geoDataJSONToText (Right (Left f))  = return $ Data.ByteString.Char8.pack $ removeNonNumbers (show (Prelude.head (_data (f :: ForwardGeoData))))
geoDataJSONToText (Right (Right f)) = return $ Data.ByteString.Char8.pack (show (lat (geometry (Prelude.head (results (f :: OpenCageForwardGeoData))))) ++ "," ++ show (lng (geometry (Prelude.head (results (f :: OpenCageForwardGeoData))))))
