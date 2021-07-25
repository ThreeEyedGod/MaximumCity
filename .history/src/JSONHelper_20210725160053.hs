
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JSONHelper where
import System.Environment
import Data.Aeson as Q
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

{-- testing 
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
    Just person -> print $ personAge $ (Prelude.take 1 person)!!0
    Nothing -> Prelude.putStrLn "Couldn't parse the JSON data"

-- end testing --}


jsonURL :: String -> Text -> String
jsonURL u q = u ++ (Data.ByteString.Char8.unpack q)

exceptionHandler ::  SomeException -> IO B.ByteString
exceptionHandler e = (putStrLn "Bad Error") >> (return B.empty)

getJSON :: String -> Text -> IO B.ByteString
getJSON url parm = simpleHttp (jsonURL url parm) `X.catch` exceptionHandler
