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
  )
where

import Polysemy
import Data.Function             ((&))
import qualified Data.Text as T

type PlaceName = T.Text 
type TheWeatherThere = T.Text

data WWI p w m a where
  GetWeatherTown :: PlaceName -> WWI PlaceName TheWeatherThere m TheWeatherThere

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
makeSem ''WWI

