{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram (handleUpdate, getTelegramSettings, TC, runTC, gettheTelegram) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
import Debug.Trace (trace)
--import Data.ByteArray (convert)
import Data.Maybe
--import Data.Text.Show
import Data.Aeson
import Control.Monad.IO.Class
import GeoIpAPI
import Helper
import Control.Monad.Catch
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
getTelegramSettings :: IO (Either String TC)
getTelegramSettings = do
  tk <- getKey "TELEGRAM_TOKEN"
  case tk of
    Left msg -> trace ("Left " ++ show msg) $ pure $ Left $ "Telegram Token error"
    Right token -> do
      let t = Token ("bot" <> T.pack token)
      manager <- newManager tlsManagerSettings
      trace ("Right  " ++ show token) $ return $ Right $ (t, manager)

{--
  To Do: got to refactor so that end is based on a long space so New York City can come in rightly
  Look into https :// hackage . haskell . org / package / text -1.2 . 4.1 / docs / Data - Text.html #g : 17
--}
gettheTelegram :: Update -> T.Text
gettheTelegram Update {message = Just m} =
    T.dropWhileEnd (==' ') (fromMaybe "" (text m))

handleUpdate :: T.Text -> Update -> TelegramClient ()
handleUpdate helper Update {message = Just m} = do
    let c = ChatId (chat_id (chat m))
    liftIO $ printf "message from %s: %s\n" (maybe "?" user_first_name (from m)) (maybe "" T.unpack (text m))
    let whatUserTyped = T.dropWhileEnd (==' ') (fromMaybe "" (text m))

    T.isPrefixOf whatUserTyped
      | "/start" || "/" = do
                          _rm1 <- sendMessageM $ sendMessageRequest c $ "Hi! I am @MaximumCityBot \nEnter your place name "
        return ()

    if ("/start" `T.isPrefixOf` whatUserTyped) || ("/" `T.isPrefixOf` whatUserTyped) 
      then do
        _rm1 <- sendMessageM $ sendMessageRequest c $ "Hi! I am @MaximumCityBot \nEnter your place name "
        return ()
      else do
        _rm2 <- sendMessageM $ sendMessageRequest c $ helper
        return ()

handleUpdate _ u  =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u

