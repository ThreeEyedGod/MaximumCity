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
import qualified Data.ByteString.Lazy as B
import System.IO.Error (isDoesNotExistError, tryIOError)

predS :: String -> Bool
predS s  = isString s 

isString :: String -> Bool
isString s = not (s == "")

statusExceptionHandler :: SomeException -> IO String
statusExceptionHandler e = show "Bad Error" >> return Data.Text.empty
    
getKey :: String -> IO (Maybe String)
getKey x = errH <$> (getEnv x `X.catch` statusExceptionHandler)

errH :: String -> Maybe String
errH s | predS s = (Just s)
       | otherwise = Nothing 
