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
import System.Environment (getEnv)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Numeric
import Data.Char (isDigit)

-- testing 
import Data.Aeson
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 (unlines)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Prelude hiding (unlines)

testJson :: ByteString
testJson = Data.ByteString.Lazy.Char8.unlines
    [ "[" 
    ,  "{"
    , "  \"age\": 25,"
    , "  \"name\": {"
    , "    \"first\": \"John\","
    , "    \"last\": \"Doe\""
    , "  }"
    , "}"
    , ","
    ,"{"
    , "  \"age\": 26,"
    , "  \"name\": {"
    , "    \"first\": \"John1\","
    , "    \"last\": \"Doe1\""
    , "  }"
    , "}"
    ,"]"
    ]

data Name = Name
    { firstName :: String
    , lastName :: String
    } deriving (Eq, Show)

data Person = Person
    { personName :: Name
    , personAge :: Int
    } deriving (Eq, Show)

instance FromJSON Name where
    parseJSON (Object v) = do
        first <- v .: "first"
        last  <- v .: "last"
        return $ Name first last
    parseJSON _ = mzero

instance FromJSON Person where
    parseJSON (Object v) = do
        nameObj <- v .: "name"
        name    <- parseJSON nameObj
        age     <- v .: "age"
        return $ Person name age


tryS :: IO ()
tryS = case decode testJson :: Maybe [Person] of
    Just person -> print $ firstName (Prelude.take 1 person)!!0
    Nothing -> Prelude.putStrLn "Couldn't parse the JSON data"

-- end testing

isPeriodorCommaorDigit :: Char -> Bool
isPeriodorCommaorDigit c = (c == '.') || (c == ',') || (isDigit c)

removeNonNumbers :: String -> String
removeNonNumbers = Prelude.filter isPeriodorCommaorDigit

-- concatenate strings to make a string
catSS :: String -> String -> String
catSS r s  = r ++ s 

-- concatenate string and a float to make a string with float precision 2
catSF :: String -> Float -> String
catSF r f = r ++ showFFloat (Just 2) f ""

-- concatenate string and int to make a string
catSI :: String -> Int -> String
catSI r i = r ++ show i

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (5 / 9) * (f - 32)

-- deriving Eq because in SpecHelper tests we are testing for equality 
data EnvError
  = MissingEnvError String
  | EmptyKeyError String
  | SomeIOError IOError
  deriving (Eq, Show)

badEnv :: String -> IOException -> IO (Either EnvError String)
badEnv env ex
      | Prelude.null env = return $ Left $ EmptyKeyError "Environment key not given "
      | isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex

-- handle is basically a guard  - a better catch !
getKey :: String -> IO (Either EnvError String)
getKey env = handle (badEnv env) $ Right <$> (getEnv env)