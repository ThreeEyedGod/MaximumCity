{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown, getWeather, PreprocessedHeaders) where
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import GHC.Integer.Logarithms ()
import OpenWeatherAPI
import PirateWeatherAPI
import IP2Location
import WaterLevelLakes
import WaterLevelHeaders
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString.UTF8 as BSU 

type PreprocessedHeaders = LB.ByteString 
type PlaceName = T.Text 
type TheWeatherThere = T.Text

getTownNameWeatherFromIp :: PreprocessedHeaders -> IO TheWeatherThere
getTownNameWeatherFromIp headers = do
  town <- extractXForwardedForHeader headers
  if "Fail" `T.isPrefixOf` town
    then do
      return $ "Fail"
    else do
      getTownNameWeatherFromTown town

getTownNameWeatherFromTown :: PlaceName -> IO TheWeatherThere
getTownNameWeatherFromTown town = do
  (region, lakelevel) <- getWaterLakeLevelForPlace_LiveToday_wrtStorage town
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  if "Fail" `T.isPrefixOf` weather1
  then do                 
    weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
    let tw = (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather2)
    return $ Data.ByteString.Char8.pack tw
  else do  
    maybe_wll <- (Data.ByteString.Char8.unpack $ TSE.decodeUtf8 $ (percent_Today lakelevel))              
    case maybe_wll of
      Nothing -> 
      Just wll -> do 
          let tw = (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ " % livelake level at " ++ (BSU.toString region) ++ " is " ++ wll )
    return $ Data.ByteString.Char8.pack tw

getWeather :: Maybe PreprocessedHeaders -> Maybe PlaceName -> IO TheWeatherThere
getWeather (Just p) Nothing = getTownNameWeatherFromIp p
getWeather _ (Just pl) = getTownNameWeatherFromTown pl 
