
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TelegramDataTypes (TMessageBody) where

import GHC.Generics (Generic)
--import Data.ByteArray (convert)

--import Servant.Client.Internal.HttpClient (ClientM(..))
--import Servant.Client.Internal

import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API.UVerb
import System.Environment
import Text.Printf
import Web.Telegram.API.Bot
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
