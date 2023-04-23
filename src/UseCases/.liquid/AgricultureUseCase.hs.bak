{-# LANGUAGE BlockArguments, GADTs, FlexibleContexts, FlexibleInstances, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# Language PartialTypeSignatures #-}
{-# Language NamedWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-totality" @-}
--{-@ LIQUID "--skip-module" @-}

module UseCases.AgricultureUseCase (getInfoTlgm, getInfoPlainText)
where

import           Polysemy ( Sem, Member, Embed, embed, Members)
import           Polysemy.Error
import           Polysemy.Input           ()
import           Polysemy.Trace           (Trace, trace)
import           UseCases.WWI             (WWI, PlaceName, TheWeatherThere, getWeatherTown, sendBackMsg, UserAsk (..), UserMsg (..))
import           InterfaceAdapters.Preferences ( modalUser, setPreferences, getPreferences, Preferences (..) )
import           InterfaceAdapters.Telegram.Telegram (
        getUserIdNumber
      , User (user_id, user_first_name , user_last_name, user_username, user_language_code)
      , getUpdate_id
      , TelegramMessage (..)
      , gettheTelegram
      , getMeta
      , getUserId
      , Update(Update, message, update_id))
import           InterfaceAdapters.Utils.Helper
import           AWSLambda (responseBody)
import qualified Data.Text.Internal as T
import qualified Data.Text as M
import Data.Either (fromRight)
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import Text.RawString.QQ
import Data.Aeson (eitherDecode)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)

import Web.Telegram.API.Bot.Data (Message, Message (..), Message(text),User(..))

-- Liquid Haskell imports
import Polysemy.Internal (Sem (..))
import qualified Data.Text.Lazy as Tx
import qualified Data.Text.Internal.Lazy as TL

getInfoTlgm :: (Member (Embed IO) r, Member WWI r) => TelegramMessage -> Sem r TheWeatherThere
getInfoTlgm updt@(Update {message = Just m})
      | (isValidPreferencesJSON . eitherDecode . encodeUtf8 . fromStrict) resp  = do -- if preferences are valid and a JSON format ....
                  apiSetTlgm updt 
                  sendBackMsg $ theMsg resp updt
                  pure . fst $ theMsg resp updt
      | otherwise = do
            if resp == tlgm then
                  case M.unpack respChecked of
                        (u1:u2:rx) -> do
                              responseBody <- apiGetTlgm (Update {update_id = updateid} {message = Just Message {text = Just $ M.pack (u1:u2:rx)}{from = Just User {user_id = getUserIdNumber (from m)}}})
                              sendBackMsg $ theMsg responseBody updt
                              pure . fst $ theMsg responseBody updt
                        _          -> do 
                              sendBackMsg $ theMsg resp updt
                              pure . fst $ theMsg resp updt
              else do
                        sendBackMsg $ theMsg resp updt
                        pure . fst $ theMsg resp updt
      where
            (uuid, (tlgm, resp)) = getMeta (Just updt)
            --resp = gettheTelegram updt
            updateid = getUpdate_id updt
            respChecked = filterInvalid resp
getInfoTlgm updt@(Update {message = Nothing}) = pure . fst $ theMsg "Update:message=nothing!!" updt

-- Domain Data
{-@ measure txtLen :: T.Text -> Int @-}
{-@ measure gettheTelegram :: TelegramMessage -> T.Text @-}
{-@ type ValidInboundMsg = {tlgm: TelegramMessage | 1 < txtLen (gettheTelegram tlgm) }  @-}

{-@ apiGetTlgm :: ValidInboundMsg -> Sem _ _ @-}
apiGetTlgm :: (Member (Embed IO) r, Member WWI r) => TelegramMessage -> Sem r TheWeatherThere
apiGetTlgm updt = do
                  thisuserprefs <- embed (getPreferences uid)
                  getWeatherTown $ UserAsk {placeName = resp, prefs = thisuserprefs}
            where 
                  (uid, (tlgm, resp)) = getMeta (Just updt)

apiSetTlgm :: (Member (Embed IO) r, Member WWI r) => TelegramMessage -> Sem r ()
apiSetTlgm updt = embed (setPreferences uid resp) where
                  (uid, (tlgm, resp)) = getMeta (Just updt)

theMsg :: T.Text -> TelegramMessage -> UserMsg
theMsg r u = (r, Just u) :: UserMsg

getInfoPlainText :: (Member (Embed IO) r, Member WWI r) => PlaceName -> Sem r TheWeatherThere
getInfoPlainText plName = embed (getPreferences modalUser) >>=  (\thisuserprefs -> getWeatherTown $ UserAsk {placeName = plName, prefs = thisuserprefs})

apiGetPlainText :: (Member (Embed IO) r, Member WWI r) => PlaceName -> Sem r TheWeatherThere
apiGetPlainText plName  = embed (getPreferences modalUser) >>=  (\thisuserprefs -> getWeatherTown $ UserAsk {placeName = plName, prefs = thisuserprefs})

apiSetPlainText :: (Member (Embed IO) r, Member WWI r) => PlaceName -> Sem r ()
apiSetPlainText plName  = pure ()

isValidPreferencesJSON :: Either String Preferences -> Bool
isValidPreferencesJSON json = case json of 
            Left invalid                          -> False 
            Right (Preferences uData uSize uSpan) -> True

{-@  filterInvalid :: x:T.Text -> o:T.Text @-}
filterInvalid :: T.Text -> T.Text
filterInvalid this = fromRight "invalid place" $ placeLike this
{-@ assume M.pack :: i:String -> {o:T.Text | len i == txtLen o } @-}
{-@ assume M.unpack :: i:T.Text -> {o:String | len o == txtLen i } @-}
{-@ assume Data.String.fromString :: x:_ -> {v:_ | v ~~ x} @-}

{-@ placeLike :: x:T.Text  -> rv : (Either T.Text {rght:T.Text | 1 < txtLen rght && txtLen x == txtLen rght}) @-}
placeLike :: T.Text -> Either T.Text T.Text
placeLike empty  = Left "Invalid"
placeLike y      = case M.unpack y of 
                        (u:v:_) -> Right y
                        _       -> Left "Invalid"  