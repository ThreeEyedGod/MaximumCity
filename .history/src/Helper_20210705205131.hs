{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

getKeyFrEnv :: String -> Maybe String
getKeyFrEnv e = do
  key <- getEnv e
  return key 
