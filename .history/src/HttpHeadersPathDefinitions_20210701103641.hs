
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HttpHeadersPathDefinitions where

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
