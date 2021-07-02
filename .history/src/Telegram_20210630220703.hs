{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram (handleUpdate, getTelegramSettings, TC, runTC) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
--import Data.ByteArray (convert)
import Data.Maybe
--import Data.Text.Show
import Data.Aeson
import Control.Monad.IO.Class
import GeoIpAPI

import Control.Monad.Catch
--import Servant.Client.Internal.HttpClient (ClientM(..))
--import Servant.Client.Internal
import Servant.API.UVerb
import Web.Telegram.API.Bot
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import Control.Monad(void)
import GeoIpAPI
import PirateWeatherAPI
import TelegramDataTypes
import qualified Data.ByteString as Data.ByteString.Char8

type TC = (Token, Manager)

runTC :: TC -> TelegramClient () -> IO ()
runTC (token, manager) act = void $ runTelegramClient token manager act

-- t.me/MaximumCityBot http api
getTelegramSettings :: IO TC
getTelegramSettings = do
  token <- getEnv "TELEGRAM_TOKEN"
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings
  return (t, manager)


handleUpdate :: T.Text -> Update -> TelegramClient ()
handleUpdate helper Update {message = Just m} = do
  let c = ChatId (chat_id (chat m))
  liftIO $ printf "message from %s: %s\n" (maybe "?" user_first_name (from m)) (maybe "" T.unpack (text m))
  let whatUserTyped = T.dropWhileEnd (==' ') (fromMaybe "" (text m))
  if "/start" `T.isPrefixOf` whatUserTyped
  then do
    _rm1 <- sendMessageM $ sendMessageRequest c $
      "Hi! I am @MaximumCityBot \n"
    _rm2 <- sendMessageM $ sendMessageRequest c $ "Enter your place name \n"
    return ()
  else do
    _rm2 <- sendMessageM $ sendMessageRequest c $ helper
    return ()

weather :: T.Text -> T.Text
weather inTown =
  do
    responseBody <- getWeatherForTown (pack )
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
      Left _ -> pure $ Right $ Lib.Response 200 responseHeaders "Not Telegram" False
      Right update -> do
        runTC tc $ handleUpdate responseBody update
        pure $ Right $ Lib.Response 200 responseHeaders "Telegram" False
  where
    responseHeaders = (object ["Access-Control-Allow-Headers" .= ("*" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)])

handleUpdate _ u =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u
