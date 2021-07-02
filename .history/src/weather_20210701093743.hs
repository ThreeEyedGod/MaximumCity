{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Weather where

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
