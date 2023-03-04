{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterfaceAdapters.Weather.Weather
(
    _getTownNameWeatherFromIp
  , _getTownNameWeatherFromTown
  , getWeather
  , PreprocessedHeaders
  , getAgInfo
) where
import Control.Monad.IO.Class ()
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
--import GHC.Integer.Logarithms ()
import Data.Text.Encoding as TSE ()
import Data.Text.Lazy.Encoding as TLE ()
import Data.ByteString.UTF8 as BSU ( toString )
import Data.Maybe ()

import InterfaceAdapters.Weather.OpenWeatherAPI
    ( getWeatherForTown )
import InterfaceAdapters.Weather.PirateWeatherAPI
    ( weatherCurrentForecastMini,
      _getWeatherForTown,
      weatherCurrentForecast )
import InterfaceAdapters.IP.IP2Location ()
import InterfaceAdapters.Water.MH.Core.WaterLevelLakes
    ( getWaterLakeLevelForPlace_LiveToday_wrtStorage )
import InterfaceAdapters.Water.MH.Core.WaterLevelHeaders
    ( PercentLiveStorage(percent_Today), Region )
import InterfaceAdapters.Utils.HttpHeadersPathDefinitions
    ( extractXForwardedForHeader )
import InterfaceAdapters.Preferences
    ( Timespan(RightNow, NearForecast),
      Datasize(Mini, Standard, Detailed),
      Agdata(WaterLevels, WeatherWaterLevels, Weather, Monsoon, All),
      Preferences(Preferences, userdata, usersize, usertimespan) )
import UseCases.WWI (UserAsk (..), PlaceName, TheWeatherThere )

type PreprocessedHeaders = LB.ByteString
type WaterLevel = String
type WeatherText = T.Text

_getTownNameWeatherFromIp :: PlaceName -> IO TheWeatherThere
_getTownNameWeatherFromIp town
  | "Fail" `T.isPrefixOf` town = return "Fail:getTownNameWeatherFromIp | town"
  | otherwise = _getTownNameWeatherFromTown town

-- | water levels of lakes in a region as percent of same time last year or full capacity
-- | from Water modules 
_helperLivePercent :: (Maybe Region, Maybe PercentLiveStorage) -> IO String
_helperLivePercent ( Nothing , _ ) = pure " No Region detectable for determining lake levels "
_helperLivePercent ( _ , Nothing ) = pure " No Water Data can be found for place provided "
_helperLivePercent (Just a, Just b) = pure $ " % livelake level at " ++ BSU.toString a ++ " is " ++ BSU.toString (percent_Today b)

_mkWeatherThere :: PlaceName -> WeatherText -> WaterLevel -> IO TheWeatherThere
_mkWeatherThere twn ("" :: T.Text)  wl = pure $ Data.ByteString.Char8.pack (Data.ByteString.Char8.unpack "" ++ wl)
_mkWeatherThere twn wt wl = pure $ Data.ByteString.Char8.pack  (Data.ByteString.Char8.unpack twn ++ " is " ++ Data.ByteString.Char8.unpack wt ++ wl )

_handlePirateResponse :: WeatherText -> PlaceName -> WaterLevel -> IO TheWeatherThere
_handlePirateResponse weather1 town wll
   | "Fail:" `T.isPrefixOf` weather1 = InterfaceAdapters.Weather.OpenWeatherAPI.getWeatherForTown (Data.ByteString.Char8.unpack town) >>= (\weather2 -> _mkWeatherThere town weather2 wll)
   | otherwise =  _mkWeatherThere town weather1 wll

_getTownNameWeatherFromTown :: PlaceName -> IO TheWeatherThere
_getTownNameWeatherFromTown town =  (getWaterLakeLevelForPlace_LiveToday_wrtStorage town >>=  _helperLivePercent) >>=
  (\wl -> InterfaceAdapters.Weather.PirateWeatherAPI._getWeatherForTown (Data.ByteString.Char8.unpack town) >>=
    (\w -> _handlePirateResponse w town wl))

getWeather :: Maybe PreprocessedHeaders -> Maybe PlaceName -> IO TheWeatherThere
getWeather (Just p) Nothing = extractXForwardedForHeader p >>= _getTownNameWeatherFromIp
getWeather _ (Just pl)      = _getTownNameWeatherFromTown pl
getWeather Nothing Nothing  = pure ("Hmmm... Something Deeply Wrong" :: TheWeatherThere)

getAgInfo ::  UserAsk -> IO TheWeatherThere
getAgInfo UserAsk {placeName = "/prefs", prefs = _ } = return $ Data.ByteString.Char8.pack "/Preferences related  "
getAgInfo UserAsk {placeName = "/start", prefs = _ } = return $ Data.ByteString.Char8.pack "/Start related  "

getAgInfo UserAsk {placeName = pl, prefs = pfs@Preferences {userdata = Weather, usersize = _, usertimespan = _}} = weatherCurrentForecast pfs $ Data.ByteString.Char8.unpack pl
-- getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = Weather, usersize = Mini, usertimespan = RightNow}} = weatherCurrentForecastMini $ Data.ByteString.Char8.unpack pl
-- getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = Weather, usersize = _, usertimespan = _}} = weatherCurrentForecastMini $ Data.ByteString.Char8.unpack pl
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WaterLevels, usersize = Mini, usertimespan = RightNow}} = (getWaterLakeLevelForPlace_LiveToday_wrtStorage pl >>= _helperLivePercent) >>= _mkWeatherThere pl ""
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WaterLevels, usersize = Mini, usertimespan = NearForecast}} = (getWaterLakeLevelForPlace_LiveToday_wrtStorage pl >>= _helperLivePercent) >>= _mkWeatherThere pl ""
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WaterLevels, usersize = _, usertimespan = _}} = (getWaterLakeLevelForPlace_LiveToday_wrtStorage pl >>= _helperLivePercent) >>= _mkWeatherThere pl ""
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WeatherWaterLevels, usersize = Mini, usertimespan = RightNow}} = _getTownNameWeatherFromTown pl
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WeatherWaterLevels, usersize = Standard, usertimespan = NearForecast}} = _getTownNameWeatherFromTown pl
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = WeatherWaterLevels, usersize = Detailed, usertimespan = NearForecast}} = _getTownNameWeatherFromTown pl
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = Monsoon, usersize = _, usertimespan = _}} = return $ Data.ByteString.Char8.pack "Sorry ! Monsoon forecast not yet available." 
getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = All, usersize = _, usertimespan = _}} = return $ Data.ByteString.Char8.pack "Sorry ! Monsoon forecast not yet ready. Weather, Water available" 

getAgInfo _ = return $ Data.ByteString.Char8.pack "Are your preferences set right? Use /prefs <Weather or WaterLevels or WeatherWaterLevels or Monsoon or All> <Mini or Standard or Detailed> <RightNow or Alerts or NearForecast or LongRange> For ex: /prefs weather mini rightnow " 
-- getAgInfo UserAsk {placeName = pl, prefs = Preferences {userdata = Weather, usersize = Mini, usertimespan = Alerts}} = weatherCurrentAlerts $ Data.ByteString.Char8.unpack pl 
