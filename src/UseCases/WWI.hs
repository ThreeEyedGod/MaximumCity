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
  , UserAsk (..)
  , WeatherStatus
  , WeatherStatusError (..)
  )
where

import Polysemy
import Data.Function             ((&))
import qualified Data.Text as T
import InterfaceAdapters.Preferences

type PlaceName = T.Text 
type TheWeatherThere = T.Text
data UserAsk = UserAsk {
  placeName :: PlaceName
, prefs     :: Preferences
}
type WeatherStatus = WWI UserAsk TheWeatherThere
-- | The functional error, raised if getting weather is not possible
newtype WeatherStatusError = WeatherStatusNotPossible String -- deriving (Show, Eq)

data WWI p w m a where
  GetWeatherTown :: UserAsk -> WWI UserAsk TheWeatherThere m TheWeatherThere

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
makeSem ''WWI