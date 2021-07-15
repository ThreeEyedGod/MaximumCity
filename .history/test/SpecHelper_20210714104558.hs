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

libH :: Spec
libH = describe "LibH" $ do
    libHBasic
    libHAdvanced
    libHProperty
  
libHBasic :: Spec
libHBasic = describe "LibHBasic" $ do        
    describe "numDigits 1000" $ do
      it "returns numDigits 1000" $ numDigits 1000 `shouldBe` 4
    describe "split_digits 1234" $ do
      it "returns split_digits 1234" $ split_digits 1234 2 `shouldBe` (12, 34)
    describe "karatsuba : 2531 * 1467 " $ do
        it "returns karatsuba 2531 x 1467 " $  karatsuba 2531 1467 `shouldBe` 3712977

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

libHAdvanced :: Spec
libHAdvanced = describe "LibHAdvanced" $ do
    describe "getKey: existing Key test" $ do
        it "getKey Key Exists Test" $ getKey "PATH" `shouldBe` "3712977"

libHProperty :: Spec
libHProperty = do
        modifyMaxSuccess (const 10) $ prop "split_digits abcd y" 
            prop_1

-- Property test 
prop_1 :: String -> Property
prop_1 (not (Prelude.null s)) = Prelude.length $ s > 0 ==> getKey s  == Left $ (MissingEnvError s) 


