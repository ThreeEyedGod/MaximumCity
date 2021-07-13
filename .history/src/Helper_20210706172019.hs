{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where
import Control.Applicative
import Control.Exception as X
import Control.Exception as X
import Control.Monad
import Data.Text
import GHC.Generics
import System.Environment
import Prelude
import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified System.IO.Error

predS :: String -> Bool
predS s  = isString s 

isString :: String -> Bool
isString s = not (s == "")

statusExceptionHandler :: SomeException -> IO String
statusExceptionHandler e 
    | e == isDoesNotExistError = "Missing in Environment"
    | otherwise  = "Something else"

getKey :: String -> IO (Maybe String)
getKey x = errH <$> (getEnv x `X.catch` statusExceptionHandler)

errH :: String -> Maybe String
errH s | predS s = (Just s)
       | otherwise = Nothing 
