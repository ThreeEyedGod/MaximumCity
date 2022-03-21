{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs,
           NamedFieldPuns, FlexibleContexts, FlexibleInstances, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module UseCases.AgricultureUseCase
( 
            getInfo
)
where

import           Polysemy

import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown, sendBackMsg, UserAsk (..), UserMsg (..))
import           InterfaceAdapters.Preferences
import           InterfaceAdapters.Telegram.Telegram
import           InterfaceAdapters.Utils.Helper
import           InterfaceAdapters.Parameters.AWSSSMParmStore (doGetParameter, doPutParameter, ParameterName (..), ParameterValue (..), ssmService)
import           InterfaceAdapters.Parameters.AWSViaHaskell


class UserInput x where 
      getInfo :: (Member (Embed IO) r, Member WWI r) => x -> Sem r TheWeatherThere

instance UserInput TelegramMessage where
  getInfo updt = do 
      thisuserprefs <- embed getPreferences
      responseBody <- getWeatherTown $ UserAsk {placeName = gettheTelegram updt, prefs = thisuserprefs} 
      let msg = (responseBody, Just updt) :: UserMsg
      sendBackMsg msg
      pure $ (fst msg)

instance UserInput PlaceName where 
      getInfo plName = getWeatherTown $ UserAsk {placeName = plName}
