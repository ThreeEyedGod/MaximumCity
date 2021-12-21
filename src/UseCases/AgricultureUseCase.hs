{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module UseCases.AgricultureUseCase
( 
    weatherTown
  , WeatherStatus
  , WeatherStatusError (..)
)
where

import           qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown)

type WeatherStatus = WWI PlaceName TheWeatherThere

-- | The functional error, raised if getting weather is not possible
newtype WeatherStatusError = WeatherStatusNotPossible String -- deriving (Show, Eq)

weatherTown :: Member WeatherStatus r => PlaceName -> Sem r TheWeatherThere
weatherTown plname = getWeatherTown plname

