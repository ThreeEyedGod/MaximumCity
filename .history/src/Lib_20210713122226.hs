
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where
import IP2Location ( doInit, doQuery, IP2LocationRecord(city) )
import GHC.Generics ( Generic )
import Data.Aeson
    ( eitherDecode,
      encode,
      (.:),
      object,
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )
import GHC.Integer.Logarithms ()
import Aws.Lambda ( Context )
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Maybe ( fromMaybe )
import Data.Text.Lazy as TL ()
import qualified Data.Text.Lazy.IO as TLO
import qualified Data.Text.Lazy.Encoding as TLO
import Network.AWS.Data.Headers ()
import Control.Exception as X ()
import Control.Monad ( MonadPlus(mzero) )
import Debug.Trace ( trace )

import Telegram ( gettheTelegram, handleUpdate, runTC, TC )
import Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown)
import Web.Telegram.API.Bot ( Update )

import qualified Data.Aeson as TLO
import HttpHeadersPathDefinitions as H

preProcessHeaders :: Value -> LB.ByteString
preProcessHeaders headers = do 
  let test = encode $ headers
  trace ("preProcessHeaders = " ++ show test) $ test

preProcessBodytoGetTelegram :: T.Text -> LB.ByteString
preProcessBodytoGetTelegram rawbody = do
    let d = eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ rawbody) :: Either String Update
    case d of
      Left _ -> "Not Telegram"
      Right theTelegram -> encode $ theTelegram

preProcessPath :: Value -> LB.ByteString
preProcessPath path  = do
  let maybePath = encode $ path
  maybePath

getPath :: LB.ByteString -> T.Text
getPath p  = do
  let f = eitherDecode p :: Either String Path
  case f of 
    Left _ -> "no path"
    Right aPath -> (ipath aPath)


data Event = Event
  { 
    path :: Value,
    headers :: Value,
    body :: Maybe T.Text
  } deriving (Generic, FromJSON)

badhandler :: String -> Event -> Context -> IO (Either String H.Response)
badhandler s Event {path, headers, body} context =
  do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
        responseBody <- s ++ (getTownNameWeatherFromIp (preProcessHeaders headers))
        pure $ Right $ H.Response 200 responseHeaders responseBody False
      Right update -> do
        responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
        pure $ Right $ H.Response 200 responseHeaders responseBody False
  where
    responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])

handler :: TC -> Event -> Context -> IO (Either String H.Response)
handler tc Event {path, headers, body} context = 
  do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
        responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
        pure $ Right $ H.Response 200 responseHeaders responseBody False
      Right update -> do
          responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
          runTC tc $ handleUpdate responseBody update 
          pure $ Right $ H.Response 200 responseHeaders responseBody False
    where
        responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
  