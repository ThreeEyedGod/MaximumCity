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
import AWSLambda (responseBody)
import InterfaceAdapters.Preferences (setPreferences)

class UserInput x where 
      getInfo :: (Member (Embed IO) r, Member WWI r) => x -> Sem r TheWeatherThere

instance UserInput TelegramMessage where
  getInfo updt
      | tlgm == resp = do 
                  thisuserprefs <- embed (getPreferences uuid)
                  responseBody <- getWeatherTown $ UserAsk {placeName = gettheTelegram updt, prefs = thisuserprefs} 
                  let msg = (responseBody, Just updt) :: UserMsg
                  sendBackMsg msg
                  pure (fst msg)
      | otherwise  = do 
                  x <- embed (setPreferences uuid resp)
                  let msg = ("Preferences have been set\n", Just updt) :: UserMsg
                  sendBackMsg msg
                  pure (fst msg)
      where 
            (uuid, (tlgm, resp)) = getMeta (Just updt) 

instance UserInput PlaceName where 
       getInfo plName = embed (getPreferences modalUser) >>=  (\thisuserprefs -> getWeatherTown $ UserAsk {placeName = plName, prefs = thisuserprefs})
