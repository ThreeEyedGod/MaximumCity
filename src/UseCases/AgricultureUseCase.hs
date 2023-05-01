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
import           Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import           Text.RawString.QQ
import           Data.Aeson (eitherDecode)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy (fromStrict)
import           InterfaceAdapters.Utils.ShortCircuit (HasTrue, Shortcircuit (..), andM, firstFalseOf, firstFalseOfM)
import           Data.Either (fromRight, isRight, partitionEithers)

import           Web.Telegram.API.Bot.Data (Message, Message (..), Message(text),User(..))

-- Liquid Haskell imports
import Polysemy.Internal (Sem (..))
import qualified Data.Text.Lazy as Tx
import qualified Data.Text.Internal.Lazy as TL
import           Data.String

getInfoTlgm :: (Member (Embed IO) r, Member WWI r) => TelegramMessage -> Sem r TheWeatherThere
getInfoTlgm updt@(Update {message = Just m})
      | (isValidPreferencesJSON . eitherDecode . encodeUtf8 . fromStrict) resp  = do -- if preferences are valid and a JSON format ....
                  apiSetTlgm updt 
                  outBoundRespond resp updt
      | otherwise = do
                  case (respChecked == "Invalid", M.unpack respChecked, length (M.unpack resp) < 29, 4 > length (words (M.unpack resp))) of
                        (True, _ , _, _)            -> outBoundRespond "Place Name is incorrectly specified" updt
                        (False, u1:u2:rx, True, True)  -> do
                              responseBody <- apiGetTlgm (Update {update_id = getUpdate_id updt} {message = Just Message {text = Just $ M.pack (u1:u2:rx)}{from = Just User {user_id = getUserIdNumber (from m)}}})
                              outBoundRespond responseBody updt
                        _                        ->  outBoundRespond resp updt
      where
            (uuid, (tlgm, resp)) = getMeta (Just updt)
            respChecked = rejectInvalid resp
getInfoTlgm updt@(Update {message = Nothing}) = pure . fst $ theMsg "Update:message=nothing!!" updt

outBoundRespond :: (Member WWI r)  => T.Text -> TelegramMessage -> Sem r TheWeatherThere
outBoundRespond r u = (sendBackMsg $ theMsg r u) >> pure r

{-@ measure txtLen :: T.Text -> Int @-}

{-@ measure numSpaces :: T.Text -> Int @-}
numSpaces :: T.Text -> Int 
numSpaces t = (length $ words (M.unpack t)) - 1 


{-@ measure gettheTelegram :: TelegramMessage -> T.Text @-}
{-@ predicate BtwnXclu Lo V Hi = (Lo < V && V < Hi) @-}
{-@ predicate Lt X Y = X < Y        @-}
{-@ predicate Ge X Y = not (Lt X Y) @-}
{-@ predicate Lte X Y = X <= Y        @-}
{-@ predicate Gt X Y = not (Lte X Y) @-}
{-@ predicate Ne X Y = X /= Y @-}
{-@ type TextNE = {v:T.Text | 0 < txtLen v} @-}
{-@ type LegitMultiWordPlaceName = { v:T.Text | Lte (numSpaces v) 3}  @-}

{-@ type ValidInboundMsg = {tlgm: TelegramMessage | Lte (numSpaces (gettheTelegram tlgm)) 3 && BtwnXclu 1 (txtLen (gettheTelegram tlgm)) 29 }  @-}

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
            where (rejectionsLeft,oksRight) = partitionEithers [placeLikeMinText this, placeLikeMaxText this, placeHasExcessiveWords this]

{-@ assume M.pack :: i:String -> {o:T.Text | len i == txtLen o } @-}
{-@ assume M.unpack :: i:T.Text -> {o:String | len o == txtLen i } @-}
{-@ assume Data.String.fromString :: x:_ -> {v:_ | v ~~ x} @-}
{-@ assume words :: i:String -> {o:[String] | (len i) - len o >= 0 } @-}


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

-- Gopalpur on the sea is the valid one I can think of, so 4
{-@ placeHasExcessiveWords :: x:T.Text  -> rv : (Either T.Text {rght:T.Text | txtLen x == txtLen rght}) @-}
placeHasExcessiveWords :: T.Text -> Either T.Text T.Text
placeHasExcessiveWords y      = case words $ M.unpack y of 
                        (a:b:c:d:e:_) -> Left "Invalid" 
                        _             -> Right y                        