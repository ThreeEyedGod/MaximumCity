
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

import Telegram ( gettheTelegram, _handleUpdate, runTC, TC, gettheTelegramMaybe)
import Weather (_getTownNameWeatherFromIp, _getTownNameWeatherFromTown, getWeather, PreprocessedHeaders)
import Web.Telegram.API.Bot ( Update )

import qualified Data.Aeson as TLO
import HttpHeadersPathDefinitions as H
import qualified Network.HTTP.Types as H1

type StatusCode = Int
type ResponseBody = T.Text
type AllInputs  = (PreprocessedHeaders, Maybe Update) 

preProcessHeaders :: Value -> PreprocessedHeaders
preProcessHeaders headers = encode headers 

preProcessBodytoGetTelegram :: T.Text -> LB.ByteString
preProcessBodytoGetTelegram rawbody = do
    let d = eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ rawbody) :: Either String Update
    case d of
      Left _ -> "Fail:preProcessBodytoGetTelegram | Not Telegram"
      Right theTelegram -> encode $ theTelegram

preProcessPath :: Value -> LB.ByteString
preProcessPath path  = encode path 

getPath :: LB.ByteString -> T.Text
getPath p  = do
  let f = eitherDecode p :: Either String Path
  case f of 
    Left _ -> "Fail:getPath | No path"
    Right aPath -> (ipath aPath)

processApiGatewayRequest :: Maybe TC -> Event -> Context context -> IO (LambdaResult 'APIGatewayHandlerType)
processApiGatewayRequest tc Event {path, headers, body} context = do  
  (responseBody, update) <- mkRespBodygetUpdate Event {path, headers, body}   
  runTC tc $ _handleUpdate responseBody update -- | this function handles response to Telegram
  pure $ formAPIGatewayResult 200 responseBody          


mkRespBodygetUpdate :: Event -> IO (ResponseBody, Maybe Update)
mkRespBodygetUpdate Event {path, headers, body} = getWeather (Just (fst a)) (gettheTelegramMaybe (snd a)) >>= (\responseBody -> pure (responseBody, snd a))
    where a = getHdrBdyMaybe Event {path, headers, body} 

{- mkRespBodygetUpdate :: Event -> IO (ResponseBody, Maybe Update)
mkRespBodygetUpdate Event {path, headers, body} = do
    let a = getHdrBdyMaybe Event {path, headers, body} 
    responseBody <- getWeather (Just (fst a)) (gettheTelegramMaybe (snd a))
    pure (responseBody, snd a)
 -}
getHdrBdyMaybe :: Event -> (PreprocessedHeaders, Maybe Update) 
getHdrBdyMaybe Event {path, headers, body} = (preProcessHeaders headers, TLO.decode $ (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))))

formAPIGatewayResult ::  StatusCode -> ResponseBody -> LambdaResult 'APIGatewayHandlerType
formAPIGatewayResult statusCode responseBody = APIGatewayResult $ mkApiGatewayResponse statusCode responseHeaders responseBodyText
  where 
    responseHeaders :: H1.ResponseHeaders = [("Access-Control-Allow-Headers","*"), ("Content-Type","application/json"), ("Access-Control-Allow-Origin","*"), ("Access-Control-Allow-Methods", "POST,GET,OPTIONS")]
    responseBodyText :: ApiGatewayResponseBody = ApiGatewayResponseBody responseBody


