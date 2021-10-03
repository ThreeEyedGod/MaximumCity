{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram (_handleUpdate, getTelegramSettings, TC, runTC, gettheTelegram, gettheTelegramMaybe, getTelegram) where
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
import Debug.Trace (trace)
--import Data.ByteArray (convert)
import Data.Maybe
import Data.Either
--import Data.Text.Show
import Control.Monad.IO.Class
import Helper
import Web.Telegram.API.Bot
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad(void)
--import TelegramDataTypes
import qualified Data.ByteString as Data.ByteString.Char8
import Data.Aeson
    ( eitherDecode,
      encode,
      KeyValue((.=)),
      object,
      Value (Object)
       )

type TC = (Token, Manager)

runTC :: Maybe TC -> TelegramClient () -> IO ()
runTC Nothing _ = pure () -- | really nothing can be done vis-a-vis telegram!
runTC (Just (token, manager)) act = void $ runTelegramClient token manager act

-- t.me/MaximumCityBot http api
getTelegramSettings :: IO (Either String TC)
getTelegramSettings = do
  tk <- getKey "TELEGRAM_TOKEN"
  case tk of
    Left msg -> trace ("Left " ++ show msg) $ pure $ Left $ "Fail:getTelegramSettings | Telegram Token error"
    Right token -> do
      let t = Token ("bot" <> T.pack token)
      manager <- newManager tlsManagerSettings
      trace ("Right  " ++ show token) $ return $ Right $ (t, manager)

{--
  To Do: got to refactor so that end is based on a long space so New York City can come in rightly
  Look into https :// hackage . haskell . org / package / text -1.2 . 4.1 / docs / Data - Text.html #g : 17
--}
gettheTelegram :: Update -> T.Text
gettheTelegram Update {message = Just m} = T.dropWhileEnd (==' ') (fromMaybe "" (text m))

gettheTelegramMaybe :: Maybe Update -> Maybe T.Text
gettheTelegramMaybe Nothing = Nothing
gettheTelegramMaybe (Just u) = Just (gettheTelegram u)


getTelegram :: Maybe T.Text -> Maybe T.Text
getTelegram tape = rightToMaybe $ eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" tape)))

{- getTelegram :: Maybe T.Text -> Maybe T.Text
getTelegram tape = do 
    case eitherDecode (LB.fromStrict (T.encodeUtf8 (fromMaybe "" tape))) of
        Left _ -> Nothing 
        Right msg -> Just msg 
 -}
_pushTelegramMsg :: T.Text -> ChatId -> TelegramClient ()
_pushTelegramMsg msg cid  = (sendMessageM $ sendMessageRequest cid $ msg) >> return ()

_handleUpdate :: T.Text -> Maybe Update -> TelegramClient ()
_handleUpdate helper (Just Update {message = Just m}) 
  | hlpAsk == False  = _pushTelegramMsg helper c
  | otherwise        = _pushTelegramMsg hlpMsg c
  where
      c = ChatId (chat_id (chat m))
      whatUserTyped = T.dropWhileEnd (==' ') (fromMaybe "" (text m))
      hlpAsk = ("/start" `T.isPrefixOf` whatUserTyped) || ("?" `T.isPrefixOf` whatUserTyped) || ("/Help" `T.isPrefixOf` whatUserTyped) || ("Help" `T.isPrefixOf` whatUserTyped) :: Bool
      hlpMsg = "Hi! I am @MaximumCityBot \nEnter your place name \nEnter For ex: \nMumbai, \nPune \nMaharashtra\n Bhivandi\n " :: T.Text

_handleUpdate _ u  = liftIO $ putStrLn $ "Unhandled message: " ++ show u
