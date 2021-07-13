{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

getKeyFrEnv :: String -> Maybe String
getKeyFrEnv "" = Nothing

getKeyFrEnv e = 
  key <- getEnv e
  case key of 
    Nothing = return "Fail"
