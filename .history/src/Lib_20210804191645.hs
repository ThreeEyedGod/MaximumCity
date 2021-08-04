
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

import Telegram ( gettheTelegram, handleUpdate, runTC, TC )
import Weather (getTownNameWeatherFromIp, getTownNameWeatherFromTown)
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

{--
    To Do: Need to factor in s into the response body
--}
badhandler :: String -> Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
badhandler s Event {path, headers, body} context =
  do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
        responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
        let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
        pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
      Right update -> do
        responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
        let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
        pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
  where
    responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]

handler :: TC -> Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
handler tc Event {path, headers, body} context = 
  do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
        responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
        let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
        pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
      Right update -> do
          responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
          let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
          runTC tc $ handleUpdate responseBody update 
          pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
    where
      responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]

processRequest :: String -> TC -> Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
handler s tc Event {path, headers, body} context = 
  do
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> do
        responseBody <- getTownNameWeatherFromIp (preProcessHeaders headers)
        let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
        pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
      Right update -> do
          responseBody <- getTownNameWeatherFromTown (gettheTelegram update)
          let responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
          if (not (Prelude.null (unpack s)) then 
            runTC tc $ handleUpdate responseBody update 
          pure . APIGatewayResult $ mkApiGatewayResponse 200 responseHeaders responseBodyText
    where
      responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]
