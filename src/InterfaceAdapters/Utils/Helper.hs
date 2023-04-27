{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--skip-module" @-}



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
import System.IO (stdout, hFlush)
import System.IO.Error (isDoesNotExistError, tryIOError, ioeGetErrorString)
import System.Environment (getEnv)
import Numeric
import Data.Char (isDigit)
import GHC.Stack

type InFunction = String
type CalleeFunction = String
type ErrLeftString = String
type KeyString = String

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

isPeriodorCommaorDigit :: Char -> Bool
isPeriodorCommaorDigit c = (c == '.') || (c == ',') || isDigit c

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
  -- | SomeIOError IOError
  | SomeIOError String
  deriving (Eq, Show)


-- interesting ! see properties of Either at https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Either.html
-- either is foldable in Data.foldable and has a method of foldr
stanEitherFoldr :: Either Int Int -> Int
stanEitherFoldr = Prelude.foldr (+) 0

getKeyEither :: Either EnvError String -> String
getKeyEither (Left _) = "getKey Error "
getKeyEither (Right str) = str

badEnv :: String -> String -> IOException -> IO (Either EnvError KeyString)
badEnv cs env ex
      | Prelude.null env =  do
          let err = "Environment key not set: " ++ env ++ "; " ++ cs
          logMessage err
          pure $ Left $ EmptyKeyError err
      | isDoesNotExistError ex = do
          let err = "Environment key value not set: " ++ env ++ "; " ++ cs
          logMessage err
          pure $ Left $ MissingEnvError err
      | otherwise = do
          let err = ioeGetErrorString ex ++ " " ++ env ++ "; " ++ cs
          logMessage err
          pure $ Left $ SomeIOError err

-- handle is basically a guard  - a shorter catch ! https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:handle
-- note: badEnv has 2 arguments; 2nd one is the exception
-- reminder: <$> is fmap https://stackoverflow.com/questions/37286376/what-does-mean-in-haskell/37286470
getKey :: HasCallStack => String -> IO (Either EnvError String)
getKey env = handle (badEnv (prettyCallStack callStack) env) $ Right <$> getEnv env

orDieonNothing :: Maybe a -> String -> Either String a
Just a  `orDieonNothing` _      = pure a
Nothing `orDieonNothing` string = Left string

key :: String -> IO (Either ErrLeftString String)
key k = do
  tk <- getKey k
  case tk of
    Left _ -> pure $ Left $ "Fail " ++ k ++ " | Token error"
    Right token -> pure $ Right token


-- composeMaybe unused as of now
(<..>) ::  (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f <..> g = \x ->
  case f x of
    Nothing -> Nothing
    Just x' -> g x'

-- compose Two Functions in order with the same argument returning second Function
(<...>) ::  (a -> b) -> (a -> c) -> a -> c
(<...>) f g = f >> g 

_returnStdFail :: InFunction -> CalleeFunction -> Text
_returnStdFail withinFunction calledFunction = pack $ "Fail:" ++ withinFunction ++ " | " ++ calledFunction

logMessage :: String -> IO ()
logMessage s = putStrLn s >> hFlush stdout

