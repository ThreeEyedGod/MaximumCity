{-# LANGUAGE BlockArguments, GADTs, FlexibleContexts, FlexibleInstances, DataKinds, PolyKinds, ScopedTypeVariables #-}
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
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import Text.RawString.QQ
import Data.Aeson (eitherDecode)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import InterfaceAdapters.Utils.ShortCircuit (HasTrue, Shortcircuit (..), andM, firstFalseOf, firstFalseOfM)
import Data.Either (fromRight, isRight, partitionEithers)

import Web.Telegram.API.Bot.Data (Message, Message (..), Message(text),User(..))

-- Liquid Haskell imports
import Polysemy.Internal (Sem (..))
import qualified Data.Text.Lazy as Tx
import qualified Data.Text.Internal.Lazy as TL

getInfoTlgm :: (Member (Embed IO) r, Member WWI r) => TelegramMessage -> Sem r TheWeatherThere
getInfoTlgm updt@(Update {message = Just m})
      | (isValidPreferencesJSON . eitherDecode . encodeUtf8 . fromStrict) resp  = do -- if preferences are valid and a JSON format ....
                  apiSetTlgm updt 
                  outBoundRespond resp updt
      | M.unpack respChecked /= "Invalid" = do
                  case (M.unpack respChecked, length (M.unpack respChecked) < 29) of
                        (u1:u2:rx, True)  -> do
                              responseBody <- apiGetTlgm (Update {update_id = getUpdate_id updt} {message = Just Message {text = Just $ M.pack (u1:u2:rx)}{from = Just User {user_id = getUserIdNumber (from m)}}})
                              outBoundRespond responseBody updt
                        _          ->  outBoundRespond "Place Name is of incorrect length" updt
      | otherwise      = outBoundRespond resp updt
      where
            (uuid, (tlgm, resp)) = getMeta (Just updt)
            respChecked = rejectInvalid resp
getInfoTlgm updt@(Update {message = Nothing}) = pure . fst $ theMsg "Update:message=nothing!!" updt

outBoundRespond :: (Member WWI r)  => T.Text -> TelegramMessage -> Sem r TheWeatherThere
outBoundRespond r u = (sendBackMsg $ theMsg r u) >> pure r

{-@ measure txtLen :: T.Text -> Int @-}
{-@ measure gettheTelegram :: TelegramMessage -> T.Text @-}
{-@ predicate BtwnXclu Lo V Hi = (Lo < V && V < Hi) @-}
{-@ type ValidInboundMsg = {tlgm: TelegramMessage | BtwnXclu 1 (txtLen (gettheTelegram tlgm)) 29 }  @-}

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

-- TO DO - use shortcircuit to do only one of the Left's !
{-@  rejectInvalid :: x:T.Text -> o:T.Text @-}
rejectInvalid :: T.Text -> T.Text
rejectInvalid this = case (rejectionsLeft,oksRight) of 
                  ([], x:_) -> x -- i.e no (Left _) rejections and at least one (Right _) List item
                  _         -> "Invalid" 
            where (rejectionsLeft,oksRight) = partitionEithers [placeLikeMinText this, placeLikeMaxText this]

{-@ assume M.pack :: i:String -> {o:T.Text | len i == txtLen o } @-}
{-@ assume M.unpack :: i:T.Text -> {o:String | len o == txtLen i } @-}
{-@ assume Data.String.fromString :: x:_ -> {v:_ | v ~~ x} @-}

{-@ placeLikeMinText :: x:T.Text  -> rv : (Either T.Text {rght:T.Text | 1 < txtLen rght && txtLen x == txtLen rght}) @-}
placeLikeMinText :: T.Text -> Either T.Text T.Text
placeLikeMinText y      = case M.unpack y of 
                        (u:v:_) -> Right y
                        _       -> Left "Invalid"  

-- 28 char long Venkatanarasimharajuvaripeta is a place in Andhra Pradesh
{-@ placeLikeMaxText :: x:T.Text  -> rv : (Either T.Text {rght:T.Text | 29 > txtLen rght && txtLen x == txtLen rght}) @-}
placeLikeMaxText :: T.Text -> Either T.Text T.Text
placeLikeMaxText y      = case M.unpack y of 
                        (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a1:a2:a3:_) -> Left "Invalid"
                        _       -> Right y