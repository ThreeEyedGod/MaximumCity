
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HttpHeadersPathDefinitions where
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:),
  )
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Monad (MonadPlus (mzero))

data Path = Path 
  {
    ipath :: T.Text
  } deriving (Generic)
instance FromJSON Path
instance ToJSON Path

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
  } deriving (Generic, FromJSON)