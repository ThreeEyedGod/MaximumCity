{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
-- the following two directives are needed for AWS Lambda 
-- to compile and work 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | this module is deprecated since the usage of servant

module ExternalInterfaces.Lib 
( 
    processApiGatewayRequest
) where
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
import qualified Network.HTTP.Types as H1
import Web.Telegram.API.Bot ( Update )
import qualified Data.Aeson as TLO

import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H
import InterfaceAdapters.Telegram.Telegram ( gettheTelegram, _handleUpdate, runTC, TC, gettheTelegramMaybe, _callTelegramClient)
import InterfaceAdapters.Weather.Weather (_getTownNameWeatherFromIp, _getTownNameWeatherFromTown, getWeather, PreprocessedHeaders)
import InterfaceAdapters.IP.IP2Location ( doInit, doQuery, IP2LocationRecord(city) )

type StatusCode = Int

preProcessHeaders :: Value -> PreprocessedHeaders
preProcessHeaders headers = encode headers 

processApiGatewayRequest :: Maybe TC -> H.Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
processApiGatewayRequest tc H.Event {path, headers, body} context = mkRespBodygetUpdate H.Event {path, headers, body} >>= 
  \ain -> (_callTelegramClient tc ain) >> (pure $ formAPIGatewayResult 200 (fst ain))

mkRespBodygetUpdate :: H.Event -> IO (H.ResponseBody, Maybe Update)
mkRespBodygetUpdate H.Event {path, headers, body} = getWeather (Just (fst a)) (gettheTelegramMaybe (snd a)) >>= (\responseBody -> pure (responseBody, snd a))
    where a = getHdrBdyMaybe H.Event {path, headers, body} 

getHdrBdyMaybe :: H.Event -> (PreprocessedHeaders, Maybe Update) 
getHdrBdyMaybe H.Event {path, headers, body} = (preProcessHeaders headers, TLO.decode $ (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))))

formAPIGatewayResult ::  StatusCode -> H.ResponseBody -> LambdaResult 'APIGatewayHandlerType
formAPIGatewayResult statusCode responseBody = APIGatewayResult $ mkApiGatewayResponse statusCode responseHeaders responseBodyText
  where 
    responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]
    responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody
