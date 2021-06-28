{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram (handleUpdate, getTelegramSettings, TC, runTC, TMessageBody) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
--import Data.ByteArray (convert)
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Catch
--import Servant.Client.Internal.HttpClient (ClientM(..))
--import Servant.Client.Internal
import Servant.API.UVerb
import Web.Telegram.API.Bot
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import Control.Monad(void)

type TC = (Token, Manager)

data MessageMeta = FromMessageMeta
  {
    id :: Int,
    is_bot :: Bool, --false
    first_name :: T.Text,
    last_name :: T.Text,
    language_code :: T.Text --en
  } 
  deriving (Generic)
data ChatTextMeta = ChatTextMeta
  {
    id_chat :: Int,
    first_name_chat :: T.Text,
    last_name_chat :: T.Text,
    type_chat :: T.Text --private 
  } deriving (Generic)

data EntityDetails = EntityDetails {
  offsetD :: Int,
  lengthD :: Int,
  typeD :: T.Text --bot_command
} deriving (Generic)

data MessageDetails  = MessageDetails {
    fromMeta :: MessageMeta,
    chatMeta :: ChatTextMeta,
    dateMessage ::  Int,
    actualMessage :: T.Text,
    allEntityDetails :: [EntityDetails]
} deriving (Generic)

data TMessageBody = TMessageBody
 {
    update_id :: Int,
    message :: MessageDetails
 } deriving (Generic)


runTC :: TC -> TelegramClient () -> IO ()
runTC (token, manager) act = void $ runTelegramClient token manager act

-- t.me/MaximumCityBot http api
getTelegramSettings :: IO TC
getTelegramSettings = do
  token <- getEnv "TELEGRAM_TOKEN"
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings
  return (t, manager)

handleUpdate :: T.Text  -> Update -> TelegramClient ()
handleUpdate helper Update{ message = Just m } = do
  let c = ChatId (chat_id (chat m))
  liftIO $ printf "message from %s: %s\n" (maybe "?" user_first_name (from m)) (maybe "" T.unpack (text m))
  if "/start" `T.isPrefixOf` fromMaybe "" (text m)
  then do
    _rm1 <- sendMessageM $ sendMessageRequest c $
      "Hi! I am @MaximumCityBot \n"
    _rm2 <- sendMessageM $ sendMessageRequest c $ helper

   -- _rm2 <- sendGameM $ sendGameRequest (fromIntegral (chat_id (chat m))) "kaleidogen"

    return ()
  else
      return ()

handleUpdate _ u =
  liftIO $ putStrLn $ "Unhandled message: " ++ show u

--deriving instance MonadMask ClientM
