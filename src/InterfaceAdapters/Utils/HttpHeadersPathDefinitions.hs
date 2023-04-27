{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterfaceAdapters.Utils.HttpHeadersPathDefinitions
where
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    encode,
    object,
    (.:),
    eitherDecode
  )
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad (MonadPlus (mzero))
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8
import Debug.Trace
import InterfaceAdapters.IP.IP2Location

data Path = Path
  {
    ipath :: T.Text
  } deriving (Generic)
instance FromJSON Path
instance ToJSON Path

type ResponseBody = T.Text

data Headers = Headers
  {
    xForwardedFor :: T.Text
  } deriving (Show, Generic)

instance FromJSON Headers where
  parseJSON (Object v) = do
    xForwardedFor <- v .: "X-Forwarded-For"
    return $ Headers xForwardedFor
  parseJSON _ = mzero

instance ToJSON Headers where
  toJSON (Headers xForwardedFor) = object ["xForwardedFor" .= xForwardedFor]

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
  } deriving (Generic, FromJSON, ToJSON)

extractXForwardedForHeader :: LB.ByteString -> IO T.Text
extractXForwardedForHeader headers = do
  let f = eitherDecode headers :: Either String Headers
  case f of
    Left _ -> trace ("f = " ++ show f) $ pure "Fail:extractXForwardedForHeader | eitherDecode header"
    Right allHeaders -> getInfoFromIpAddr (T.unpack (xForwardedFor allHeaders))
    --Right allHeaders -> getInfoFromIpAddr (Data.ByteString.Char8.unpack (xForwardedFor allHeaders))

preProcessPath :: Value -> LB.ByteString
preProcessPath = encode

getPath :: LB.ByteString -> T.Text
getPath p  = do
  let f = eitherDecode p :: Either String Path
  case f of
    Left _ -> "Fail:getPath | No path"
    Right aPath -> ipath aPath
