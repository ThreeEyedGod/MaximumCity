{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module InterfaceAdapters.Telegram.Telegram (
    _handleUpdate
  , getTelegramSettings
  , TC
  , runTC
  , gettheTelegram
  , getTelegram
  , getTelegramUser
  , getUserId
  , _callTelegramClient
  , TelegramMessage
  , parseGetResponse
  , parsePrefs
  , getMeta
) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf ()
import Debug.Trace (trace)
import Data.Maybe ( fromMaybe )
import Data.Either.Combinators ( rightToMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import InterfaceAdapters.Utils.Helper ( getKey )
import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H
    ( ResponseBody )
import           Data.Monoid ((<>))
import InterfaceAdapters.Preferences (parsePrefs, )

import Web.Telegram.API.Bot
    ( Update(Update, message),
      Token(..),
      TelegramClient,
      ChatId(ChatId),
      runTelegramClient,
      sendMessageM,
      sendMessageRequest,
      Chat(chat_id),
      Message(from, chat, text),
      User (user_id, user_first_name , user_last_name, user_username, user_language_code), 
      SetWebhookRequest (webhook_allowed_updates))

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad(void)
import qualified Data.ByteString as Data.ByteString.Char8
import Data.Aeson
    ( eitherDecode,
      encode,
      decode,
      KeyValue((.=)),
      object,
      FromJSON,
      parseJSON,
      Value (Object, Bool)
       )
import qualified Data.Text.Lazy.Lens as T

type TC = (Token, Manager)
type AllInputs  = (H.ResponseBody, Maybe Update)
type TelegramMessage = Update


runTC :: Maybe TC -> TelegramClient () -> IO ()
runTC Nothing _ = pure () -- | really nothing can be done vis-a-vis telegram!
runTC (Just (token, manager)) act = void $ runTelegramClient token manager act

getTelegramSettings :: IO (Either String TC)
getTelegramSettings = do
  tk <- getKey "TELEGRAM_TOKEN"
  case tk of
    Left msg -> trace ("Left " ++ show msg) $ pure $ Left "Fail:getTelegramSettings | Telegram Token error"
    Right token -> do
      let t = Token ("bot" <> T.pack token)
      manager <- newManager tlsManagerSettings
      trace ("Right  " ++ show token) $ return $ Right (t, manager)

_callTelegramClient :: Maybe TC -> AllInputs -> IO ()
_callTelegramClient tc allin = runTC tc $ uncurry _handleUpdate allin

{--
  To Do: got to refactor so that end is based on a long space so New York City can come in rightly
  Look into https :// hackage . haskell . org / package / text -1.2 . 4.1 / docs / Data - Text.html #g : 17
--}
gettheTelegram :: Update -> T.Text
gettheTelegram Update {message = Just m} = T.dropWhileEnd (==' ') (fromMaybe "" (text m))
gettheTelegram _ = ""

getTelegramUser :: Update -> Maybe User
getTelegramUser Update {message = Just m} = from m
getTelegramUser _  = Nothing

getUserId :: Maybe User -> T.Text
getUserId (Just u) = T.pack . show $ user_id u
getUserId Nothing = ""

getTelegram :: Maybe T.Text -> Maybe T.Text
getTelegram tape = rightToMaybe $ eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" tape)))

-- | pushes a message to Telegram Chatid
_pushTelegramMsg :: T.Text -> ChatId -> TelegramClient ()
_pushTelegramMsg msg cid  = void (sendMessageM $ sendMessageRequest cid msg)

_handleUpdate :: T.Text -> Maybe Update -> TelegramClient ()
_handleUpdate helper (Just Update {message = Just m}) = _pushTelegramMsg helper $ ChatId (chat_id (chat m))
_handleUpdate _ u = liftIO $ putStrLn $ "Unhandled message: " ++ show u

getMeta :: Maybe Update -> (T.Text, (T.Text, T.Text))
getMeta (Just Update {message = Just m}) = (uuid, (whatUserTyped, parseResponse))
  where
    uuid = getUserId (from m)
    whatUserTyped = T.toLower $ T.dropWhileEnd (== ' ') (fromMaybe "" (text m))
    parseResponse = parseGetResponse whatUserTyped uuid
getMeta Nothing = ("getMeta no data", ("usertyped missing","so no response"))
getMeta _ = ("getMeta unkwown error", ("usertyped bad", "so no response"))

parseGetResponse :: T.Text -> T.Text -> T.Text
parseGetResponse whatUserTyped uuid
  | ("/start" `T.isPrefixOf` whatUserTyped) || ("?" `T.isPrefixOf` whatUserTyped) || ("/help" `T.isPrefixOf` whatUserTyped) || ("help" `T.isPrefixOf` whatUserTyped) = hlpMessage
  | "/prefs" `T.isPrefixOf` whatUserTyped = prfsMessage
  | ("/" `T.isInfixOf` whatUserTyped) || ("*" `T.isInfixOf` whatUserTyped) || ("-" `T.isInfixOf` whatUserTyped) = "\nPlace Name seems odd\n"
  | otherwise = whatUserTyped
  where
    hlpMessage = "Hi! I am @MaximumCityBot \nEnter a place name For ex: \nMumbai, \nPune \nMaharashtra \nBhivandi\n " :: T.Text
    prfsMessage = parsePrefs uuid (T.strip $ T.drop 6 whatUserTyped) -- over at @module Preferences