
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module OpenWeatherAPI where
import Data.Aeson as Q
import Data.Text
import Control.Applicative
import Control.Monad
import JSONHelper
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Exception as X
import GHC.Generics
import Control.Monad (mapM_)
import Prelude
import qualified Data.Text as Data.ByteString.Char8

data DataPoint = DataPoint { description :: String
                           } deriving (Show, Generic)

data TempPoint = TempPoint { temp :: Float
                           } deriving (Show, Generic)

data Temperatures = Temperatures { weather :: [DataPoint],
                                   test :: TempPoint
                                 } deriving (Show, Generic)

instance FromJSON Temperatures where {
  parseJSON = withObject "" $ \o ->
    Temperatures <$> o .: "weather" <*> o .: "main"
}
instance ToJSON Temperatures where {
  toJSON p = object [
    "weather" .= weather p,
    "main" .= test p]
}
instance FromJSON DataPoint
instance ToJSON DataPoint
instance FromJSON TempPoint
instance ToJSON TempPoint

jsonURL :: String -> String
jsonURL q = "http://api.openweathermap.org/data/2.5/weather?q=" ++ q ++ "&units=metric&appid=e7b9dd9f41c3ac26eae9e94536c8075e&lang=en"

getJSON :: String -> IO B.ByteString
getJSON town = simpleHttp (OpenWeatherAPI.jsonURL town) `X.catch` exceptionHandler

getWeatherForTown :: String -> IO Text
getWeatherForTown town = do
  d <- (eitherDecode <$> (OpenWeatherAPI.getJSON town)) :: IO (Either String Temperatures)
  case d of
    Left e ->  return $ "Fail " <> pack e 
    Right stuff -> return $ Data.ByteString.Char8.pack (show (temp (test stuff)) ++ " Celsius and " ++ (description (Prelude.head (weather stuff))))
