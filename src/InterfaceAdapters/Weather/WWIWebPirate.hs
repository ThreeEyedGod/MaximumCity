{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module InterfaceAdapters.Weather.WWIWebPirate
  ( 
    runWWIWebPirate
  ) 
where

import Polysemy

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import GHC.Integer.Logarithms ()
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString.UTF8 as BSU 
import qualified Data.Aeson as TLO (decode)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Maybe
import Data.Aeson
    ( decode,
      FromJSON,
      ToJSON,
      eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )
import Web.Telegram.API.Bot ( Update )

import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H
import InterfaceAdapters.Weather.PirateWeatherAPI
import UseCases.WWI
import UseCases.AgricultureUseCase
import InterfaceAdapters.Telegram.Telegram
import qualified InterfaceAdapters.Weather.Weather as IWW

runWWIWebPirate :: (Member (Embed IO) r) => Sem (WWI PlaceName TheWeatherThere : r) a -> Sem r a
runWWIWebPirate = interpret (\(GetWeatherTown req) -> embed (interfaceWebPirate req))

interfaceWebPirate :: PlaceName -> IO TheWeatherThere
interfaceWebPirate pn = IWW.getWeather Nothing (Just pn)
