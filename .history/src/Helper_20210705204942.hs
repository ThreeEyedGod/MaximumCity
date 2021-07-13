{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

getKeyFrEnv :: Text -> Maybe text
get
  key <- getEnv "API_POSITIONSTACK_KEY" -- set this in AWS Env. Variables
