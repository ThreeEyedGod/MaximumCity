{-# LANGUAGE BlockArguments, GADTs, FlexibleContexts, FlexibleInstances, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

{-@ LIQUID "--skip-module" @-}

module UseCases.AgricultureUseCase (getInfo)
where

import           Polysemy ( Sem, Member, Embed, embed )
import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown, sendBackMsg, UserAsk (..), UserMsg (..))
import           InterfaceAdapters.Preferences ( modalUser, setPreferences, getPreferences, Preferences (..) )
import           InterfaceAdapters.Telegram.Telegram ( TelegramMessage, gettheTelegram, getMeta )
import           InterfaceAdapters.Utils.Helper
import           AWSLambda (responseBody)
import qualified Data.Text as T
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import Text.RawString.QQ
import Data.Aeson (eitherDecode)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)

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
      | (isValidPreferencesJSON . eitherDecode . encodeUtf8 . fromStrict) resp  = do -- if preferences are valid and a JSON format ....
      --      | "{" `T.isInfixOf` resp  = do -- if preferences are valid and a JSON format ....
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

jsonValid     = [r| { "userdata": "Weather", "usersize": "Mini", "usertimespan": "RightNow" } |] :: String
jsonInvalid   = [r| { "userdata1": "Weather", "usersize": "Mini", "usertimespan": "RightNow" } |] :: String

isValidPreferencesJSON :: Either String Preferences -> Bool
isValidPreferencesJSON json = case json of 
            Left invalid                          -> False 
            Right (Preferences uData uSize uSpan) -> True

test1 :: [Bool]
test1 = Prelude.map (isValidPreferencesJSON . eitherDecode . fromString) [jsonValid, jsonInvalid]
