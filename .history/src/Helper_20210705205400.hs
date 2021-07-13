{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

getKeyFrEnv :: String -> Maybe String
getKeyFrEnv ""
getKeyFrEnv e = do
  key <- getEnv e
  case key of 
    Nothing = return "Fail"
