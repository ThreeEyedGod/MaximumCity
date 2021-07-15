{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck
import Helper
import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Process
import Data.Either

libH :: Spec
libH = describe "LibH" $ do
    libHBasic
    libHAdvanced
    libHProperty
  
libHBasic :: Spec
libHBasic = describe "LibHBasic" $ do        
    describe "getKey " $ do
      it "returns for colorterm" $ getKey "COLORTERM" `shouldBe` "truecolor"
{--
testEventBodyOK  :: Event
testEventBodyOK = Event {headers = "", body = "{\"firstInteger\": 2531,\"secondInteger\": 1467}"}
testEventBodyOKResponse :: IO (Either String Response)
testEventBodyOKResponse = return 
    $ Right $ Response {statusCode = 200, headers = (object ["Access-Control-Allow-Headers" .= ("Content-Type" :: String), "Content-Type" .= ("application/json" :: String), "Access-Control-Allow-Origin" .= ("*" :: String), "Access-Control-Allow-Methods" .= ("POST,GET,OPTIONS" :: String)]), body = "3712977", isBase64Encoded = False}

testJSON_OKHeader_BS :: LB.ByteString
testJSON_OKHeader_BS = "{\"X-Forwarded-For\": \"127.0.0.1, 127.0.0.2\"}"

testJSON_OKHeader :: Value
testJSON_OKHeader  = "{\"X-Forwarded-For\": \"127.0.0.1, 127.0.0.2\"}"

testJSON_NOTOKHeader_BS :: LB.ByteString
testJSON_NOTOKHeader_BS = "{\"X-Forwarded-Fo\": \"127.0.0.1, 127.0.0.2\"}"

testJSON_NOTOKHeader :: Value
testJSON_NOTOKHeader = "{\"X-Forwarded-Fo\": \"127.0.0.1, 127.0.0.2\"}"
--}

libHAdvanced :: Spec
libHAdvanced = describe "LibHAdvanced" $ do
    describe "getKey: existing Key test" $ do
        it "getKey Key Exists Test" $ getKey "COLORTERM" `shouldBe` "truecolor"

libHProperty :: Spec
libHProperty = do
        modifyMaxSuccess (const 10) $ prop "getKey Random missing keys" 
            prop_1

-- Property test 
prop_1 :: String -> Property
prop_1 s = not (s == "") ==> 
    x <- getkey 
    isLeft (getKey s) == True