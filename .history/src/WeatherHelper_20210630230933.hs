{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WeatherHelper where
import System.Environment
import Data.Aeson as Q
import Aws.Lambda (Context)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Integer.Logarithms ()
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

getTownNameWeatherFromPlace :: String -> IO T.Text
getTownNameWeatherFromPlace place = do
  weather1 <- PirateWeatherAPI.getWeatherForTown  $ place
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ place
  let tw = show (Data.ByteString.Char8.unpack place ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)
  return $ Data.ByteString.Char8.pack tw
