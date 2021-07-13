{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where
import Control.Applicative
import Control.Exception as X
import Control.Monad
import Control.Monad (mapM_)
import qualified Data.ByteString.Lazy as B
import Data.Text
import qualified Data.Text as Data.ByteString.Char8
import GHC.Float as SF
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import System.Environment
import Prelude
import System.Environment


getKey :: String -> IO (Maybe String)
getKey x = 