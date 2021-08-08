{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown) where
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import GHC.Integer.Logarithms ()
import OpenWeatherAPI
import PirateWeatherAPI
import IP2Location

newtype PreprocessedHeaders = PreprocessedHeaders LB.ByteString 
newtype PlaceName = PlaceName T.Text 
newtype TheWeatherThere = TheWeatherThere T.Text

getTownNameWeatherFromIp :: LB.ByteString -> IO T.Text
getTownNameWeatherFromIp headers = do
  town <- extractXForwardedForHeader headers
  if "Fail" `T.isPrefixOf` town
    then do
      return $ "Fail"
    else do
      getTownNameWeatherFromTown town

getTownNameWeatherFromTown :: T.Text -> IO T.Text
getTownNameWeatherFromTown town = do
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  if "Fail" `T.isPrefixOf` weather1
  then do                 
    weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
    let tw = (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather2)
    return $ Data.ByteString.Char8.pack tw
  else do                
    let tw = (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1)
    return $ Data.ByteString.Char8.pack tw

getWeather :: Maybe PreprocessedHeaders -> Maybe PlaceName -> IO TheWeatherThere
getWeather (Just p) Nothing = getTownNameWeatherFromIp p
getWeather _ (Just pl) = getTownNameWeatherFromTown pl 
