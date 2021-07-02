{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WeatherHelper where
import System.Environment
import Data.Aeson as Q
import Data.Text
import GeoIpAPI
import JSONHelper
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8
import PirateWeatherAPI
import OpenWeatherAPI

getTownNameWeatherFromIp :: LB.ByteString -> IO T.Text
getTownNameWeatherFromIp headers = do
  town <- extractXForwardedForHeader headers
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  let tw = show (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)    
  return $ Data.ByteString.Char8.pack tw

getTownNameWeatherFromPlace :: String -> IO T.Text
getTownNameWeatherFromTown place = do
  town <- extractXForwardedForHeader headers
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  let tw = show (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)
  return $ Data.ByteString.Char8.pack tw