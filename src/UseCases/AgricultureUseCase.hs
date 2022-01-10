{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module UseCases.AgricultureUseCase
( 
    weatherTown
)
where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown, WeatherStatus, UserAsk)
import           InterfaceAdapters.Preferences
{- 
type User = User String 
-- | Persistence is a key/value store Day / [Reservation]
type Persistence = KVS User Preferences
 -}
-- | getWeatherTown is in WWI and it's mapped to a functon in runWWI__
weatherTown :: Member (WWI UserAsk TheWeatherThere) r => UserAsk -> Sem r TheWeatherThere
weatherTown ua = getWeatherTown ua

{- fetch :: (Member Persistence r, Member Trace r) => User -> Sem r (Maybe Preferences)
fetch user = do
  trace $ "fetch preferences for " ++ show user
  maybePrefs <- getKvs day
  return maybePrefs
 -}