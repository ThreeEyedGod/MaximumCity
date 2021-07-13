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

data EnvError
  = MissingEnvError String
  | EmptyKeyError String
  | SomeIOError IOError
  deriving (Show)

badEnv3 :: String -> IOException -> IO (Either EnvError String)
badEnv3 env ex
      | not (predS env) = return $ Left $ EmptyKeyError "Environment key not given "
      | isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex

getEnv3 :: String -> IO (Either EnvError String)
getEnv3 env = handle (badEnv3 env) $ Right <$> (getEnv env)


predS :: String -> Bool
predS s  = isString s 

isString :: String -> Bool
isString s = not (s == "")


