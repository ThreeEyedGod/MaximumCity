{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

getKeyFrEnv :: String -> Maybe String
getKeyFrEnv "" = Nothing

getKeyFrEnv e = 
  case (getEnv e) of 
    Nothing -> return "Fail"
