{-# LANGUAGE BlockArguments, GADTs, FlexibleContexts, FlexibleInstances, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-@ LIQUID "--skip-module" @-}

module UseCases.AgricultureUseCase
(
      getInfo
)
where

import Polysemy ( Sem, Member, Embed, embed )

import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown, sendBackMsg, UserAsk (..), UserMsg (..))
import InterfaceAdapters.Preferences
    ( modalUser, setPreferences, getPreferences )
import InterfaceAdapters.Telegram.Telegram
    ( TelegramMessage, gettheTelegram, getMeta )
import           InterfaceAdapters.Utils.Helper
import AWSLambda (responseBody)
import qualified Data.Text as T
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
      | "{" `T.isInfixOf` resp  = do -- if preferences are valid and a JSON format ....
                  x <- embed (setPreferences uuid resp)
                  let msg = (resp, Just updt) :: UserMsg
                  sendBackMsg msg
                  pure (fst msg)
      | otherwise  = do
                  let msg = (resp, Just updt) :: UserMsg
                  sendBackMsg msg
                  pure (fst msg)
      where
            (uuid, (tlgm, resp)) = getMeta (Just updt)

instance UserInput PlaceName where
       getInfo plName = embed (getPreferences modalUser) >>=  (\thisuserprefs -> getWeatherTown $ UserAsk {placeName = plName, prefs = thisuserprefs})
