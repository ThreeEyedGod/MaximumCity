{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where
import Control.Applicative
import Control.Exception as X
import Control.Exception (IOException, handle, throw, catch)
import Control.Monad
import Data.Text
import GHC.Generics
import System.Environment
import Prelude
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as B
import qualified System.IO.Error (isDoesNotExistError, tryIOError)
import System.Environment (getEnv)
import Control.Monad.Except (ExceptT (..), runExceptT)

-- deriving Eq because in SpecHelper tests we are testing for equality 
data EnvError
  = MissingEnvError String
  | EmptyKeyError String
  | SomeIOError IO
  deriving (Eq, Show)

badEnv :: String -> IOException -> IO (Either EnvError String)
badEnv env ex
      | Prelude.null env = return $ Left $ EmptyKeyError "Environment key not given "
      | SIO.isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex

-- handle is basically a guard  - a better catch !
getKey :: String -> IO (Either EnvError String)
getKey env = handle (badEnv env) $ Right <$> (getEnv env)