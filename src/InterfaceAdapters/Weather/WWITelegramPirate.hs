{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveDataTypeable  #-}


module InterfaceAdapters.Weather.WWITelegramPirate
  ( runWWITelegramPirate
  , weatherTownTelegram
  ) 
where

import Polysemy
import Polysemy.Error

import qualified Control.Exception as CE
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import GHC.Integer.Logarithms ()
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString.UTF8 as BSU 
import qualified Data.Aeson as TLO (decode, encode)

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

import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H
import InterfaceAdapters.Weather.PirateWeatherAPI
import UseCases.WWI
import UseCases.AgricultureUseCase
import InterfaceAdapters.Telegram.Telegram
import qualified InterfaceAdapters.Weather.Weather as IWW

runWWITelegramPirate :: (Member (Embed IO) r) => Sem (WWI PlaceName TheWeatherThere : r) a -> Sem r a
runWWITelegramPirate = interpret (\(GetWeatherTown req) -> embed (interfaceTelegramPirate req))

interfaceTelegramPirate :: PlaceName -> IO TheWeatherThere
interfaceTelegramPirate pl = IWW.getWeather Nothing (Just pl)

weatherTownTelegram :: (Member (Embed IO) r , Member WeatherStatus r, Member (Error WeatherStatusError) r) => TelegramMessage -> Sem r TheWeatherThere
weatherTownTelegram updt = do 
      responseBody <- (weatherTown . gettheTelegram) updt
      let ain = (responseBody, Just updt)
      res <- embed getTelegramSettings
      case res of
            Left err -> CE.throw (InternalError $ "Telegram " ++ err)
            Right tk -> embed (_callTelegramClient (Just tk) ain)
      pure $ (fst ain)

-- | exceptions that may occur during Weather/Telegram operations
data WeatherTelegramException = EntityNotFound String
    | EntityAlreadyExists String
    | InternalError String
    deriving (Show)

instance CE.Exception WeatherTelegramException