{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

import System.Environment

getKey :: String -> IO Maybe String
getKey "" = Nothing 
getKey tp = do
    key <- getEnv tp 
    return Just key
