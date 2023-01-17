{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-@ LIQUID "--skip-module" @-}


module InterfaceAdapters.Weather.WWITelegramPirate
  (
      runWWITelegramPirate
  )
where

import Polysemy ( Sem, Member, Embed, embed, interpret )
import Polysemy.Error ()

import qualified Control.Exception as CE
import Control.Monad.IO.Class ()
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
--import GHC.Integer.Logarithms ()
import Data.Text.Encoding as TSE ()
import Data.Text.Lazy.Encoding as TLE ()
import Data.ByteString.UTF8 as BSU ()
import qualified Data.Aeson as TLO (decode, encode)
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

import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H ()
import InterfaceAdapters.Weather.PirateWeatherAPI ()
import UseCases.WWI ( TheWeatherThere, UserAsk, UserMsg, WWI(..) )
import UseCases.AgricultureUseCase ()
import InterfaceAdapters.Telegram.Telegram
    ( _callTelegramClient, getTelegramSettings )
import qualified InterfaceAdapters.Weather.Weather as IWW
import InterfaceAdapters.Preferences ()

runWWITelegramPirate :: (Member (Embed IO) r) => Sem (WWI ': r) a -> Sem r a
runWWITelegramPirate = interpret $ \case
  GetWeatherTown req -> embed (interfaceTelegramPirate req)
  SendBackMsg msg -> embed (sendBackTelegram msg)

interfaceTelegramPirate :: UserAsk -> IO TheWeatherThere
--interfaceTelegramPirate forthis@UserAsk {placeName = pl, prefs = Preferences {userdata = WeatherWaterLevels, usersize = Mini, usertimespan = RightNow}}  = IWW.getAgInfo forthis
interfaceTelegramPirate = IWW.getAgInfo

sendBackTelegram :: UserMsg -> IO ()
sendBackTelegram backMsg = do
      --res <- embed getTelegramSettings
      res <- getTelegramSettings
      case res of
            Left err -> CE.throw (InternalError $ "Telegram " ++ err)
            -- Right tk -> embed (_callTelegramClient (Just tk) backMsg)
            Right tk -> _callTelegramClient (Just tk) backMsg
      pure ()

-- | exceptions that may occur during Weather/Telegram operations
data WeatherTelegramException = EntityNotFound String
    | EntityAlreadyExists String
    | InternalError String
    deriving (Show)

instance CE.Exception WeatherTelegramException