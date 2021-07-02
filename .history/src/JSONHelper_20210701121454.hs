JSON
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JSONHelper where
import System.Environment
import Data.Aeson as Q
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

jsonURL :: String -> Text -> String
jsonURL u q = u ++ (Data.ByteString.Char8.unpack q)

exceptionHandler ::  SomeException -> IO B.ByteString
exceptionHandler e = (putStrLn "Bad Error") >> (return B.empty)

getJSON :: String -> Text -> IO B.ByteString
getJSON url parm = simpleHttp (jsonURL url parm) `X.catch` exceptionHandler
