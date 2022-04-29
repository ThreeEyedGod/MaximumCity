{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module InterfaceAdapters.Telegram.Telegram (
    _handleUpdate
  , getTelegramSettings
  , TC
  , runTC
  , gettheTelegram
  , gettheTelegramMaybe
  , getTelegram
  , _callTelegramClient
  , TelegramMessage
) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
import Debug.Trace (trace)
import Data.Maybe
import Data.Either.Combinators
import Control.Monad.IO.Class
import InterfaceAdapters.Utils.Helper
import InterfaceAdapters.Utils.HttpHeadersPathDefinitions as H

import Web.Telegram.API.Bot
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad(void)
import qualified Data.ByteString as Data.ByteString.Char8
import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )

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

preProcessBodytoGetTelegram :: T.Text -> LB.ByteString
preProcessBodytoGetTelegram rawbody = do
    let d = eitherDecode (LB.fromChunks . return . T.encodeUtf8 $ rawbody) :: Either String Update
    case d of
      Left _ -> "Fail:preProcessBodytoGetTelegram | Not Telegram"
      Right theTelegram -> encode theTelegram

{--
  To Do: got to refactor so that end is based on a long space so New York City can come in rightly
  Look into https :// hackage . haskell . org / package / text -1.2 . 4.1 / docs / Data - Text.html #g : 17
--}
gettheTelegram :: Update -> T.Text
gettheTelegram Update {message = Just m} = T.dropWhileEnd (==' ') (fromMaybe "" (text m))

-- | Maybe version of the getthetelegram
gettheTelegramMaybe :: Maybe Update -> Maybe T.Text
gettheTelegramMaybe Nothing = Nothing
gettheTelegramMaybe (Just u) = Just (gettheTelegram u)

getTelegram :: Maybe T.Text -> Maybe T.Text
getTelegram tape = rightToMaybe $ eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" tape)))

-- | pushes a message to Telegram Chatid
_pushTelegramMsg :: T.Text -> ChatId -> TelegramClient ()
_pushTelegramMsg msg cid  = void (sendMessageM $ sendMessageRequest cid msg)

_handleUpdate :: T.Text -> Maybe Update -> TelegramClient ()
_handleUpdate helper (Just Update {message = Just m})
  | not hlpAsk  = _pushTelegramMsg helper c
  | otherwise        = _pushTelegramMsg hlpMsg c
  where
      c = ChatId (chat_id (chat m))
      whatUserTyped = T.dropWhileEnd (==' ') (fromMaybe "" (text m))
      hlpAsk = ("/start" `T.isPrefixOf` whatUserTyped) || ("?" `T.isPrefixOf` whatUserTyped) || ("/Help" `T.isPrefixOf` whatUserTyped) || ("Help" `T.isPrefixOf` whatUserTyped) :: Bool
      hlpMsg = "Hi! I am @MaximumCityBot \nEnter your place name \nEnter For ex: \nMumbai, \nPune \nMaharashtra\n Bhivandi\n " :: T.Text

_handleUpdate _ u  = liftIO $ putStrLn $ "Unhandled message: " ++ show u
