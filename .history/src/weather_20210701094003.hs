{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Weather where
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import GHC.Integer.Logarithms ()
import IP2Location
import OpenWeatherAPI
import PirateWeatherAPI

getTownNameWeatherFromIp :: LB.ByteString -> IO T.Text
getTownNameWeatherFromIp headers = do
  town <- extractXForwardedForHeader headers
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  let tw = show (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)    
  return $ Data.ByteString.Char8.pack tw

getTownNameWeatherFromTown :: T.Text -> IO T.Text
getTownNameWeatherFromTown town = do
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  let tw = show (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)
  return $ Data.ByteString.Char8.pack tw
