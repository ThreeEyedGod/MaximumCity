{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where
import Control.Applicative
import Control.Exception as X
import Control.Monad
import Control.Monad (mapM_)
import Data.Aeson
import Data.Aeson as Q
import Data.Aeson.Internal
import Data.Aeson.Parser.Internal
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.Char (isDigit)
import Data.Text
import qualified Data.Text as Data.ByteString.Char8
import GHC.Float as SF
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import System.Environment
import Prelude
import System.Environment

result :: String  -> IO (Maybe String)
result x = errorH <$> getEnv x 


errorH :: String -> Maybe String
errorH s
  | pred s = Just (f s)
  | otherwise = Nothing
