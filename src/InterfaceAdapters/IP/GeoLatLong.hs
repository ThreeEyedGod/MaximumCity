
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-@ LIQUID "--skip-module" @-}

module InterfaceAdapters.IP.GeoLatLong (getLatLongforThis) where
import Data.Aeson
    ( 
      FromJSON,
      ToJSON,
      eitherDecode
       )
import Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Prelude
import qualified Data.Text as Data.ByteString.Char8
import InterfaceAdapters.Utils.Helper
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE
import Data.Char as DC
import InterfaceAdapters.IP.GeoCodeOpenCage (getOpenCageForwardGeoCodefor, OpenCageForwardGeoData (..), OpenCageResultData(..), OpenCageLocdata(..))
import InterfaceAdapters.IP.GeoIpAPI (getPositionStackForwardGeoCodefor, ForwardGeoData (..))
import Data.Either ( fromRight, isLeft, isRight )
import InterfaceAdapters.Utils.ShortCircuit as SC

getLatLongforThis :: String -> IO T.Text
getLatLongforThis town = getLLData town >>= geoDataToText

getLLData :: String ->  IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
getLLData s = SC.orM (fGDFromPlacePre s >>= fGDFromPlace) (oCFGDFromPlacePre s >>= oCFGDFromPlace)

fGDFromPlacePre :: String -> IO (Either String ForwardGeoData)
fGDFromPlacePre s = eitherDecode <$> getPositionStackForwardGeoCodefor s

fGDFromPlace :: Either String ForwardGeoData -> IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
fGDFromPlace (Left x)  = pure $ Left x
fGDFromPlace (Right y) = pure $ Right $ Left y 

oCFGDFromPlacePre :: String -> IO (Either String OpenCageForwardGeoData)
oCFGDFromPlacePre s = eitherDecode <$> getOpenCageForwardGeoCodefor s

oCFGDFromPlace :: Either String OpenCageForwardGeoData -> IO (Either String (Either ForwardGeoData OpenCageForwardGeoData))
oCFGDFromPlace (Left x)  = pure $ Left x
oCFGDFromPlace (Right y) = pure $ Right $ Right y 

geoDataToText :: Either String (Either ForwardGeoData OpenCageForwardGeoData) -> IO T.Text
geoDataToText (Left f)          = return $ T.pack f
geoDataToText (Right (Left f))  = return $ Data.ByteString.Char8.pack $ removeNonNumbers (show (Prelude.head (_data (f :: ForwardGeoData))))
geoDataToText (Right (Right f)) = return $ Data.ByteString.Char8.pack (show (lat (geometry (Prelude.head (results (f :: OpenCageForwardGeoData))))) ++ "," ++ show (lng (geometry (Prelude.head (results (f :: OpenCageForwardGeoData))))))
