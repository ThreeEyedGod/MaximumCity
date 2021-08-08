
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
-- the following two directives are absolutely needed for AWS Lambda 
-- to compile and work 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


module Lib where
import IP2Location ( doInit, doQuery, IP2LocationRecord(city) )
import GHC.Generics ( Generic )
import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )
import GHC.Integer.Logarithms ()
import Aws.Lambda
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Maybe ( fromMaybe )
import Data.Text.Lazy as TL ()
import qualified Data.Text.Lazy.IO as TLO
import qualified Data.Text.Lazy.Encoding as TLO
import Control.Exception as X ()
import Control.Monad ( MonadPlus(mzero) )
import Debug.Trace ( trace )
import Control.Error

import Telegram ( gettheTelegram, handleUpdate, runTC, TC, gettheTelegramMaybe)
import Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown, getWeather, PreprocessedHeaders)
import Web.Telegram.API.Bot ( Update )

import qualified Data.Aeson as TLO
import HttpHeadersPathDefinitions as H
import qualified Network.HTTP.Types as H1


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

{- | take inputs of a telegram token, an Event and a Context
     make a response body and extract the actual telegram "update"
     create standard http response header for 200 + API Gateway
     call a telegram channel message back function
     return complete standard http response back for API gateway 
-}
processApiGatewayRequest :: Maybe TC -> Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
processApiGatewayRequest tc Event {path, headers, body} context = do  
  (responseBody, update) <- mkRespBodygetUpdate Event {path, headers, body}   
  let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
  let responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]
  runTC tc $ handleUpdate responseBody update         
  pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText

{- | take inputs as an Event
     extract the actual telegram "update" from the body of the Event
     If body is malformed or missing then use the headers to go get weather for IP location in the header
      and create a response body
     else of body's good go get weather for place in the telegram "update"
      and create a response body
     return as a pair (responsebody and telegram update message)
-}


{- mkRespBodygetUpdate :: Event -> IO (T.Text, Maybe Update)
mkRespBodygetUpdate Event {path, headers, body} = do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
          responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
          pure (responseBody, Nothing)
      Right update -> do 
          responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
          pure (responseBody, Just update)
 -}
 
{- mkRespBodygetUpdate :: Event -> IO (T.Text, Maybe Update)
mkRespBodygetUpdate Event {path, headers, body} = do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
          responseBody <- getWeather (Just (preProcessHeaders headers)) Nothing
          pure (responseBody, Nothing)
      Right update -> do 
          responseBody <- getWeather Nothing (Just (gettheTelegram update))
          pure (responseBody, Just update)
 -}
mkRespBodygetUpdate :: Event -> IO (T.Text, Maybe Update)
mkRespBodygetUpdate Event {path, headers, body} = do
    let a = getHdrBdyMaybe Event {path, headers, body} 
    (hdr, bdy) <- (fst . a, snd . a)
    responseBody <- getWeather (Just fst a) (gettheTelegramMaybe $ snd a)
    pure (responseBody, bdy)

getHdrBdyMaybe :: Event -> (LB.ByteString, Maybe Update) 
getHdrBdyMaybe Event {path, headers, body} = (preProcessHeaders headers, TLO.decode $ (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))))

