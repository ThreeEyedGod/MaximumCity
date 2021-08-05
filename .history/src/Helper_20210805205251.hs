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
import Prelude
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as B
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.Environment (getEnv)
import Numeric
import Data.Char (isDigit)

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

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

orDie :: Maybe a -> String -> Either String a
Just a  `orDie` _      = return a
Nothing `orDie` string = Left string

orDie :: Either a -> String -> Either String a

{- Equivalent, more explicit, implementation:

maybe `orDie` string =
    case maybe of
        Nothing -> Left string
        Just a  -> return a
-}