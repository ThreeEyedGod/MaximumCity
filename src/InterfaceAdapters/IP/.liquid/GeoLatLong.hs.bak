
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module InterfaceAdapters.IP.GeoLatLong (getLatLongforThis) where
import Data.Aeson as Q
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

getLatLongforThis :: String -> IO T.Text
getLatLongforThis town = do
    d <- (eitherDecode <$> getPositionStackForwardGeoCodefor town) :: IO (Either String ForwardGeoData)
    case d of
      Left e -> do
        -- f <- (eitherDecode <$> getGeoIpforThis) :: IO (Either String GeoIp)
        f <- (eitherDecode <$> getOpenCageForwardGeoCodefor town) :: IO (Either String OpenCageForwardGeoData)
        case f of
          Left err -> return "Fail:getLatLongforThis | eitherDecode getOpenCageForwardGeoCodefor" 
          -- Right geoipstuff_backup -> return $ Data.ByteString.Char8.pack (show (latitude (geoipstuff_backup :: GeoIp)) ++ "," ++ show (longitude (geoipstuff_backup :: GeoIp)))
          Right geo_backup -> return $ Data.ByteString.Char8.pack (show (lat (geometry (Prelude.head (results (geo_backup :: OpenCageForwardGeoData))))) ++ "," ++ show (lng (geometry (Prelude.head (results (geo_backup :: OpenCageForwardGeoData))))))
      Right geoipstuff ->
        return $
          Data.ByteString.Char8.pack $ removeNonNumbers
            (show (Prelude.head (_data (geoipstuff :: ForwardGeoData))))
