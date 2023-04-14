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
import Data.Text (compareLength)
import Data.Either (fromRight)
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import Text.RawString.QQ
import Data.Aeson (eitherDecode)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)

class UserInput x where
      getInfo :: (Member (Embed IO) r, Member WWI r) => x -> Sem r TheWeatherThere
      apiGet :: (Member (Embed IO) r, Member WWI r) => x -> Sem r TheWeatherThere
      apiSet :: (Member (Embed IO) r, Member WWI r) => x -> Sem r ()

instance UserInput TelegramMessage where
  apiGet updt = do
                  thisuserprefs <- embed (getPreferences uid)
                  getWeatherTown $ UserAsk {placeName = resp, prefs = thisuserprefs}
            where 
                  (uid, (tlgm, resp)) = getMeta (Just updt)

  apiSet updt = embed (setPreferences uid resp) where
                  (uid, (tlgm, resp)) = getMeta (Just updt)

  getInfo updt
      | tlgm == respValid = do                  
                  --thisuserprefs <- embed (getPreferences uuid)
                  --responseBody <- getWeatherTown $ UserAsk {placeName = respValid, prefs = thisuserprefs}
                  responseBody <- apiGet updt
                  sendBackMsg $ theMsg responseBody updt
                  pure . fst $ theMsg responseBody updt
      | (isValidPreferencesJSON . eitherDecode . encodeUtf8 . fromStrict) resp  = do -- if preferences are valid and a JSON format ....
                  x <- embed (setPreferences uuid resp)
                  sendBackMsg $ theMsg resp updt
                  pure . fst $ theMsg resp updt
      | otherwise  = do
                  sendBackMsg $ theMsg resp updt
                  pure . fst $ theMsg resp updt
      where
            (uuid, (tlgm, resp)) = getMeta (Just updt)
            respValid = filterInvalid resp

theMsg :: T.Text -> TelegramMessage -> UserMsg
theMsg r u = (r, Just u) :: UserMsg

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

-- Specs: http://ucsd-progsys.github.io/liquidhaskell/specifications/
{-@ measure txtLen :: Text -> Int @-}
{-@ assume T.pack :: i:String -> {o:T.Text | len i == txtLen o } @-}
-----------------------------------------------------------
-- Domain Data
{-@ type TextPlaceLike = {v:T.Text | 17 > txtLen v} @-}

{-@  filterInvalid :: x:T.Text -> rv:TextPlaceLike @-}
filterInvalid :: T.Text -> T.Text
filterInvalid this = fromRight "" $ placeLike this

{-@ placeLike :: x:T.Text  -> rv : (Either T.Text {rght:TextPlaceLike | txtLen rght == len x})   @-}
placeLike :: T.Text -> Either T.Text T.Text
placeLike x = case compareLength x 17 of
    GT -> Left "invalid"
    _  -> Right x