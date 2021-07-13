{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

import System.Environment
getKeyFrEnv :: String -> Maybe String
getKeyFrEnv "" = Nothing

getKeyFrEnv e = 
  case (getEnv e) of 
    Nothing -> return "Fail"
