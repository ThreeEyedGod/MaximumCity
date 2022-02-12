{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module InterfaceAdapters.Utils.Helper where
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

type InFunction = String
type CalleeFunction = String
type ErrLeftString = String
type KeyString = String

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

badEnv :: String -> IOException -> IO (Either EnvError KeyString)
badEnv env ex
      | Prelude.null env = return $ Left $ EmptyKeyError "Environment key not given "
      | isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex

-- handle is basically a guard  - a shorter catch ! https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:handle
-- note: badEnv has 2 arguments; 2nd one is the exception
-- reminder: <$> is fmap https://stackoverflow.com/questions/37286376/what-does-mean-in-haskell/37286470
getKey :: String -> IO (Either EnvError String)
getKey env = handle (badEnv env) $ Right <$> (getEnv env)

orDieonNothing :: Maybe a -> String -> Either String a
Just a  `orDieonNothing` _      = return a
Nothing `orDieonNothing` string = Left string

key :: String -> IO (Either ErrLeftString String)
key k = do
  tk <- getKey k
  case tk of
    Left _ -> pure $ Left $ "Fail " ++ k ++ " | Token error"
    Right token -> pure $ Right $ token


-- composeMaybe
(<..>) ::  (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f <..> g = \x ->
  case f x of
    Nothing -> Nothing
    Just x' -> g x'

_returnStdFail :: InFunction -> CalleeFunction -> Text 
_returnStdFail withinFunction calledFunction = pack $ "Fail:" ++ withinFunction ++ " | " ++ calledFunction