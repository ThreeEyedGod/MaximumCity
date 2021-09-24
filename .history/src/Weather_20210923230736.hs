{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Weather (_getTownNameWeatherFromIp, _getTownNameWeatherFromTown, getWeather, PreprocessedHeaders) where
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import GHC.Integer.Logarithms ()
import OpenWeatherAPI
import PirateWeatherAPI
import IP2Location
import WaterLevels.MH.Core.WaterLevelLakes
import WaterLevels.MH.Core.WaterLevelHeaders
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString.UTF8 as BSU 
import Data.Maybe

type PreprocessedHeaders = LB.ByteString 
type PlaceName = T.Text 
type TheWeatherThere = T.Text
type WaterLevel = String
type WeatherText = T.Text

_getTownNameWeatherFromIp :: PlaceName -> IO TheWeatherThere
_getTownNameWeatherFromIp town
  | "Fail" `T.isPrefixOf` town = return $ "Fail:getTownNameWeatherFromIp | town"
  | otherwise = _getTownNameWeatherFromTown town

_helperLivePercent :: (Maybe Region, Maybe PercentLiveStorage) -> String
_helperLivePercent ( Nothing , _ ) = " % livelake level Not Available "
_helperLivePercent (_ , Nothing) = " % livelake level Not Available "
_helperLivePercent (Just a, Just b) = " % livelake level at " ++ (BSU.toString a) ++ " is " ++ (BSU.toString $ (percent_Today b))

_mkWeatherThere :: PlaceName -> WeatherText -> WaterLevel -> IO TheWeatherThere
_mkWeatherThere twn wt wl = pure $ Data.ByteString.Char8.pack $ (Data.ByteString.Char8.unpack twn ++ " is currently " ++ Data.ByteString.Char8.unpack wt ++ wl )

_handlePirateResponse :: WeatherText -> PlaceName -> WaterLevel -> IO TheWeatherThere
_handlePirateResponse weather1 town wll 
   | 
_handlePirateResponse "Fail" town wll = (OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town) >>= (\weather2 -> _mkWeatherThere town weather2 wll)
_handlePirateResponse weather1 town wll  =   _mkWeatherThere town weather1 wll

_getTownNameWeatherFromTown :: PlaceName -> IO TheWeatherThere
_getTownNameWeatherFromTown town = do
  (region, lakelevel) <- getWaterLakeLevelForPlace_LiveToday_wrtStorage town
  let wll = _helperLivePercent (region,lakelevel)
  (PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack town) >>= (\w -> _handlePirateResponse w town wll)

getWeather :: Maybe PreprocessedHeaders -> Maybe PlaceName -> IO TheWeatherThere
getWeather (Just p) Nothing = extractXForwardedForHeader p >>= _getTownNameWeatherFromIp 
getWeather _ (Just pl) = _getTownNameWeatherFromTown pl 
