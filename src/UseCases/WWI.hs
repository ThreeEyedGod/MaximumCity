{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module UseCases.WWI 
  ( WWI (..)
  , PlaceName
  , TheWeatherThere
  , getWeatherTown
  , sendBackMsg
  , UserAsk (..)
  , UserMsg (..)
  , WeatherStatusError (..)
 )
where

import Polysemy
import Data.Function             ((&))
import qualified Data.Text as T
import InterfaceAdapters.Preferences
import InterfaceAdapters.Telegram.Telegram

type PlaceName = T.Text 
type TheWeatherThere = T.Text
data UserAsk = UserAsk {
  placeName :: PlaceName
, prefs     :: Preferences
}
type UserMsg = (TheWeatherThere, Maybe TelegramMessage)
newtype WeatherStatusError = WeatherStatusNotPossible String -- deriving (Show, Eq)

data WWI m a where
  GetWeatherTown :: UserAsk -> WWI m TheWeatherThere
  SendBackMsg :: UserMsg -> WWI m ()

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
makeSem ''WWI