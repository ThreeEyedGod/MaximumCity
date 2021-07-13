module Main where

import Control.Exception (IOException, handle, throw)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

data MissingError
  = MissingEnv String
  | MissingFile String
  deriving (Show)

main :: IO ()
main = do
  eitherFile <- printFile
  either print print eitherFile

getEnv' :: String -> MissingError -> IO (Either MissingError String)
getEnv' env err = handle (missingEnv err) $ Right <$> (getEnv env)

readFile' :: FilePath -> MissingError -> IO (Either MissingError String)
readFile' path err = handle (missingFile err) $ Right <$> (readFile path)

missingEnv :: MissingError -> IOException -> IO (Either MissingError String)
missingEnv err = const $ return $ Left err

missingFile :: MissingError -> IOException -> IO (Either MissingError String)
missingFile err e
  | isDoesNotExistError e = return $ Left err
  | otherwise = throw e

printFile :: IO (Either MissingError String)
printFile = do
  eitherFilePath <- getEnv' "FOLDER" (MissingEnv "FOLDER")
  case eitherFilePath of
    Left err -> return $ Left err
    Right path -> readFile' path (MissingFile path)
