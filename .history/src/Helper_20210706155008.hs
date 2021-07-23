{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

import System.Environment

getKey :: String -> IO String
getKey "" = Nothing 
getKey tp = do
    key <- getEnv tp 