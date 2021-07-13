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
import System.IO.Error (isDoesNotExistError, tryIOError)
import Control.Exception (IOException, handle, throw)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Control.Monad.Except (ExceptT (..), runExceptT)

data MissingError
  = MissingEnv String
  | MissingFile String
  deriving (Show)

data EnvError
  = MissingEnvError String
  | EmptyStringError String
  | SomeIOError IOError
  deriving (Show)

badEnv :: EnvError -> IOException -> IO (Either EnvError String)
badEnv err = const $ return $ Left err

badEnv3 :: String -> IOException -> IO (Either EnvError String)
badEnv3 env ex
      | not (predS env) = return $ Left $ EmptyStringError "Environment key not given "
      | isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex

getEnv3 :: String -> IO (Either EnvError String)
getEnv3 env = handle (badEnv3 env) $ Right <$> (getEnv env)

getEnv2 :: String -> EnvError -> IO (Either EnvError String)
getEnv2 env err = handle (badEnv err) $ Right <$> (getEnv env)



predS :: String -> Bool
predS s  = isString s 

isString :: String -> Bool
isString s = not (s == "")


