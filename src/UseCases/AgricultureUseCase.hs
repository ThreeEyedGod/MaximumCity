{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, FlexibleInstances, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

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

class UserInput x where 
      getInfo :: (Member (Embed IO) r, Member WWI r) => x -> Sem r TheWeatherThere

instance UserInput TelegramMessage where
  getInfo updt = do 
      responseBody <- getWeatherTown $ UserAsk {placeName = gettheTelegram updt, prefs = Preferences {userdata = Weather, usersize = Mini, usertimespan = NearForecast}} 
      let ain = (responseBody, Just updt) :: UserMsg
      sendBackMsg ain
      pure $ (fst ain)

instance UserInput PlaceName where 
      getInfo pln = getWeatherTown $ UserAsk {placeName = pln}
