
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
    eitherDecode,
    encode,
    object,
    (.:),
  )
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
