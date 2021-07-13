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

getEnv2 :: String -> EnvError -> IO (Either EnvError String)
getEnv2 env err = handle (badEnv err) $ Right <$> (getEnv env)

getMyKey :: String -> IO (Either EnvError String)
getMyKey k = runExceptT $ do
  keyvalue <- ExceptT $ getEnv2 k (MissingEnvError k)
  ExceptT $ return $ (show keyvalue)

getEnv'' :: String -> ExceptT EnvError IO String
getEnv'' env = mapIOError env ("getting env var " ++ env) $ getEnv env

mapIOError :: String -> String -> IO a -> ExceptT EnvError IO a
mapIOError env msg = ExceptT . fmap (first (mapError env)) . tryIOError
  where
    mapError e err 
      | isDoesNotExistError err = MissingEnvError msg
      | not (predS e) = EmptyStringError msg
    mapError _ err = SomeIOError err

mapIOErrorOld :: String -> String -> IO a -> ExceptT EnvError IO a
mapIOErrorOld env msg = ExceptT . fmap (first mapError) . tryIOError
  where
    mapError err | isDoesNotExistError err = MissingEnvError msg
    mapError err = SomeIOError err

getEnv' :: String -> MissingError -> IO (Either MissingError String)
getEnv' env err = handle (missingEnv err) $ Right <$> (getEnv env)

missingEnv :: MissingError -> IOException -> IO (Either MissingError String)
missingEnv err = const $ return $ Left err

predS :: String -> Bool
predS s  = isString s 

isString :: String -> Bool
isString s = not (s == "")

statusExceptionHandler :: SomeException -> IO String
statusExceptionHandler e =  return "Bad Error"

    
getKey :: String -> IO (Maybe String)
getKey x = errH <$> (getEnv x `X.catch` statusExceptionHandler)

errH :: String -> Maybe String
errH s | predS s = (Just s)
       | otherwise = Nothing 
