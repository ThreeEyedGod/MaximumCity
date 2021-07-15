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
    --libHAdvanced
    libHProperty
  

libHBasic :: Spec
libHBasic = describe "LibHBasic" $ do        
    describe "numDigits 1000" $ do
      it "returns numDigits 1000" $ numDigits 1000 `shouldBe` 4
    describe "numDigits 54362" $ do
      it "returns numDigits 54362" $ numDigits 54362 `shouldBe` 5
    describe "numDigits 123456789" $ do
      it "returns numDigits 123456789" $ numDigits 123456789 `shouldBe` 9
    describe "split_digits 1234" $ do
      it "returns split_digits 1234" $ split_digits 1234 2 `shouldBe` (12, 34)
    describe "karatsuba : 2531 * 1467 " $ do
        it "returns karatsuba 2531 x 1467 " $  karatsuba 2531 1467 `shouldBe` 3712977
    describe "karatsuba : 12351 * 1467 " $ do
        it "returns karatsuba 12531 x 1467 " $ karatsuba 12531 1467 `shouldBe` (12531 * 1467)
    describe "karatsuba : 123 * 5 " $ do
        it "returns karatsuba 123 x 5 " $ karatsuba 123 5 `shouldBe` (123 * 5)
    describe "karatsuba : 1232531 * 51467 " $ do
        it "returns karatsuba 1232531 x 51467 " $ karatsuba 1232531 51467 `shouldBe` (1232531 * 51467)
    describe "karatsuba : 123456789 * 987654321 " $ do
        it "returns karatsuba 123456789 * 987654321 " $ karatsuba 123456789 987654321 `shouldBe` (123456789 * 987654321)
    describe "karatsuba : 234567890124464673737771818 * 1836535353547474646282828282 " $ do
        it "returns karatsuba 234567890124464673737771818 * 1836535353547474646282828282  " $ karatsuba 234567890124464673737771818 1836535353547474646282828282  `shouldBe` (234567890124464673737771818 * 1836535353547474646282828282 )

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
{---
libHAdvanced :: Spec
libHAdvanced = describe "LibHAdvanced" $ do
    --describe "preProcessHeaders OK-header" $ do
      --  it "preProcessHeaders OK header " $ preProcessHeaders testJSON_OKHeader `shouldBe` T.encodeUtf8 "{\"xForwardedFor\": \"127.0.0.1, 127.0.0.2\"}"
    -- describe "extractXForwardedForHeader OK-header" $ do
        it "extractXForwardedForHeader OK-header " $ extractXForwardedForHeader testJSON_OKHeader_BS `shouldBe` "\"127.0.0.1, 127.0.0.2\""
    describe "kText: OK-body OK Header" $ do
        it "kText OK-body" $ kText testJSON_OKHeader_BS "{\"firstInteger\": 2531,\"secondInteger\": 1467}" `shouldBe` "3712977"
    describe "kText: NotOK-body OK header" $ do
        it "kText NotOK-body" $ kText testJSON_OKHeader_BS "{\"firstInteger\": \"2531\",\"secondInteger\": 1467}" `shouldBe` "\"127.0.0.1, 127.0.0.2\""
    describe "kText: OK-body Not OK Header" $ do
        it "kText OK-body" $ kText testJSON_NOTOKHeader_BS "{\"firstInteger\": 2531,\"secondInteger\": 1467}" `shouldBe` "3712977"
    describe "kText: NotOK-body NotOK header" $ do
        it "kText NotOK-body Not OK Header" $ kText testJSON_NOTOKHeader_BS "{\"firstInteger\": \"2531\",\"secondInteger\": 1467}" `shouldBe` "bad or no xForwardedFor header"
    describe "kText: OK-body NotOK header" $ do
        it "kText OK body" $ kText "{\"X-Forwarded-For\": \"\"}" "{\"firstInteger\": 2531,\"secondInteger\": 1467}" `shouldBe` "3712977"
    describe "kText: OK-body NotOK empty header" $ do
        it "kText OK body" $ kText "{}" "{\"firstInteger\": 2531,\"secondInteger\": 1467}" `shouldBe` "3712977"
    --describe "handler Body OK" $ do
      --  it "handler testEventBodyOk empty " $ handler testEventBodyOK context `shouldBe` testEventBodyOKResponse
    --}

libHProperty :: Spec
libProperty = do
        modifyMaxSuccess (const 10) $ prop "split_digits abcd y" 
            prop_1
        modifyMaxSuccess (const 10) $ prop "numDigits "
            prop_2
        modifyMaxSuccess (const 10) $ prop "karatsuba x y " 
           prop_3
        modifyMaxSuccess (const 1000) $ prop "split_digits f " 
           prop_4

-- Property test for split_digits 
-- Check a very weak property...ha ha first half of the split digits should be greater than Zero ! Weak !
prop_1 :: Positive (Large Int) -> Property
prop_1 (Positive (Large f)) = f > 0 ==> numDigits (fst (split_digits (toInteger f) (numDigits (toInteger f) `div` 2))) > 0 

-- Another Property test for split_digits - totally unoptimized. surely a better way to write the right side 
-- first half * 10 ^ number of digits to split-by + second half should be equal to original number
prop_4 :: Positive (Large Int) -> Property
prop_4 (Positive (Large f)) = f > 0 ==> fst (split_digits (toInteger f) (numDigits (toInteger f) `div` 2)) * 10 ^ (numDigits (toInteger f) `div` 2) + (snd (split_digits (toInteger f) (numDigits (toInteger f) `div` 2))) == (toInteger f)

-- Property test for numDigits
-- A new number based on Ten raised to one less than the numDigits should be less than the original 
prop_2 :: (Positive (Large Int)) -> Property
prop_2 (Positive (Large f)) = f > 0 ==> 10 ^ (numDigits (toInteger f) - 1) <= (toInteger f)

-- Property Test for Karatsuba
-- Karatsuba against a reference naive number multiplication implementation!!
prop_3 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property 
prop_3 (Positive (Large f)) (Positive (Large s)) = f > 0 ==> traceShow ("K-value ", karatsuba (toInteger f) (toInteger s)) $ karatsuba (toInteger f) (toInteger s) == (toInteger f) * (toInteger s)



