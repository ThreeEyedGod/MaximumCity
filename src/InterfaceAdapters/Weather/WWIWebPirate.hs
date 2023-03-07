{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, BlockArguments, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE TypeFamilies        #-}
{-@ LIQUID "--skip-module" @-}

module InterfaceAdapters.Weather.WWIWebPirate ( runWWIWebPirate )
where

import Polysemy ( Sem, Member, Embed, embed, interpret )
import Polysemy.Error ()
import Control.Monad.IO.Class ()
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import Data.Text.Encoding as TSE ()
import Data.Text.Lazy.Encoding as TLE ()
import Data.ByteString.UTF8 as BSU ()
import qualified Data.Aeson as TLO (decode)
import qualified Data.Text.Encoding as T
import Data.Maybe ()
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
import UseCases.WWI ( TheWeatherThere, UserAsk(..), WWI(GetWeatherTown) )
import UseCases.AgricultureUseCase ()
import InterfaceAdapters.Weather.Weather (getWeather)
import InterfaceAdapters.Preferences ()

runWWIWebPirate :: (Member (Embed IO) r) => Sem (WWI : r) a -> Sem r a
runWWIWebPirate = interpret (\(GetWeatherTown req) -> embed (interfaceWebPirate req))

interfaceWebPirate :: UserAsk -> IO TheWeatherThere
interfaceWebPirate UserAsk {placeName = pl, prefs = _ } = getWeather Nothing (Just pl)