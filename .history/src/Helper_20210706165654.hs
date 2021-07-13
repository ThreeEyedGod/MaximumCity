{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where
import Control.Applicative
import Control.Exception as X
import Control.Monad
import Data.Text
import GHC.Generics
import System.Environment
import Prelude
import System.Environment


pred :: String -> Bool
pred s 
    | "" = False
    | otherwise 

getKey :: String -> IO (Maybe String)
getKey x = errH <$> getEnv x 

errH :: String -> Maybe String
errH s | pred s = (Just s)
