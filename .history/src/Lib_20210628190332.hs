
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where
import IP2Location
import GHC.Generics ( Generic )
import Data.Aeson
import GHC.Integer.Logarithms ()
import Aws.Lambda ( Context )
import qualified Data.Text as Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Maybe
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLO
import qualified Data.Text.Lazy.Encoding as TLO
import Network.AWS.Data.Headers
import Control.Exception as X
import Control.Monad
import Debug.Trace
import OpenWeatherAPI
import PirateWeatherAPI
import Telegram
import Web.Telegram.API.Bot

import qualified Data.Aeson as TLO

data Path = Path 
  {
    ipath :: T.Text
  } deriving (Generic)
instance FromJSON Path
instance ToJSON Path

data Headers = Headers
  { 
    xForwardedFor :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Headers where
  parseJSON (Object v) = do
    xForwardedFor <- v .: "X-Forwarded-For"
    return $ Headers xForwardedFor
  parseJSON _ = mzero

instance ToJSON Headers where
  toJSON (Headers xForwardedFor) = object ["xForwardedFor" .= xForwardedFor]

getInfoFromIpAddr :: String -> IO T.Text
getInfoFromIpAddr ipAddr = do
    let myfile = "IP2LOCATION-LITE-DB11.IPV6.BIN"
    meta <- doInit myfile
    result <- doQuery myfile meta ipAddr
    return $ Data.ByteString.Char8.pack (city result)

preProcessHeaders :: Value -> LB.ByteString
preProcessHeaders headers = do 
  let test = encode $ headers
  trace ("preProcessHeaders = " ++ show test) $ test

preProcessBody :: T.Text -> LB.ByteString
preProcessBody rawbody = do
    let d = eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ rawbody) :: Either String Update
    case d of
      Left _ -> "Perhaps Telegram"
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

extractXForwardedForHeader :: LB.ByteString -> IO T.Text
extractXForwardedForHeader headers = do
    let f = eitherDecode headers :: Either String Headers
    case f of
      Left _ -> trace ("f = " ++ show f) $ return $ "bad or no xForwardedFor header"
      Right allHeaders -> getInfoFromIpAddr (Data.ByteString.Char8.unpack (xForwardedFor allHeaders))


getTownNameWeather :: LB.ByteString -> IO T.Text
getTownNameWeather headers = do
  town <- extractXForwardedForHeader headers
  weather1 <- PirateWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  weather2 <- OpenWeatherAPI.getWeatherForTown $ Data.ByteString.Char8.unpack $ town
  let tw = show (Data.ByteString.Char8.unpack town ++ " is currently " ++ Data.ByteString.Char8.unpack weather1 ++ Data.ByteString.Char8.unpack weather2)    
  return $ Data.ByteString.Char8.pack tw

kText :: LB.ByteString -> T.Text -> IO T.Text
kText headers rawbody = do
    let d = eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ rawbody) :: Either String Update
    case d of
      Left _ -> getTownNameWeather headers
      Right theTelegram -> return $ Data.ByteString.Char8.pack (show (update_id theTelegram))

data Response = Response
  { statusCode :: Int,
    headers :: Value,
    body :: T.Text,
    isBase64Encoded :: Bool
  } deriving (Generic, ToJSON)

data Event = Event
  { 
    path :: Value,
    headers :: Value,
    body :: Maybe T.Text
  } deriving (Generic, FromJSON)

{--
handler :: TC -> Event -> Context -> IO (Either String Lib.Response)
handler tc Event {path, headers, body} context 
    | (preProcessBody (fromMaybe "" body)) == "Perhaps Telegram" =
          case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
            Left err -> pure $ Right Lib.Response
                { statusCode = 400
                , headers = object [
                    "Content-Type" .= ("text/plain" :: String)
                ]
                , isBase64Encoded = False
                , body = "Could not decode message: " <> T.pack err
                }
            Right update -> do
                responseBody <- (kText (preProcessHeaders headers) (fromMaybe "" body))
                runTC tc $ handleUpdate responseBody update
                -- runTC tc $ handleUpdate Nothing update
                pure $ Right Lib.Response
                    { statusCode = 200
                    , headers = object [
                        "Content-Type" .= ("text/plain" :: String)
                    ]
                    , isBase64Encoded = False
                    , body = "Done"
                    }
  -- almost definitely telegram
   | otherwise = do
      responseBody <- (kText (preProcessHeaders headers) (fromMaybe "" body))
      -- case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      case eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ (fromMaybe "" body)) of
          Left err -> do
            let responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
            pure $ Right $ Lib.Response 200 responseHeaders responseBody False
          Right update -> do 
            runTC tc $ handleUpdate responseBody update
            let responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
            pure $ Right $ Lib.Response 200 responseHeaders responseBody False
--/

handler :: TC -> Event -> Context -> IO (Either String Lib.Response)
handler tc Event {path, headers, body} context
  | (preProcessBody (fromMaybe "" body)) == "Perhaps Telegram" =
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left err ->
        pure $
          Right
            Lib.Response
              { statusCode = 400,
                headers =
                  object
                    [ "Content-Type" .= ("text/plain" :: String)
                    ],
                isBase64Encoded = False,
                body = "Could not decode message: " <> T.pack err
              }
      Right update -> do
        responseBody <- (kText (preProcessHeaders headers) (fromMaybe "" body))
        runTC tc $ handleUpdate responseBody update
        -- runTC tc $ handleUpdate Nothing update
        pure $
          Right
            Lib.Response
              { statusCode = 200,
                headers =
                  object
                    [ "Content-Type" .= ("text/plain" :: String)
                    ],
                isBase64Encoded = False,
                body = "Done"
              }
  -- almost definitely telegram
  | otherwise = do
    responseBody <- (kText (preProcessHeaders headers) (fromMaybe "" body))
    -- case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
    case eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ (fromMaybe "" body)) of
      Left err -> do
        let responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
        pure $ Right $ Lib.Response 200 responseHeaders responseBody False
      Right update -> do
        runTC tc $ handleUpdate responseBody update
        let responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])
        pure $ Right $ Lib.Response 200 responseHeaders responseBody False
