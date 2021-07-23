{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (Property, arbitrary, quickCheck, (==>), forAll)
import Helper
import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Process
import Data.Either as DE
import Test.RandomStrings
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

libH :: Spec
libH = describe "LibH" $ do
    --libHBasic
    --libHAdvanced
    libHProperty
  
{--libHBasic :: Spec
libHBasic = describe "LibHBasic" $ do        
    describe "getKey " $ do
      it "returns Right for colorterm" $ (rights $ getKey "COLORTERM") `shouldBe` "truecolor"
      --}

libHProperty :: Spec
libHProperty = do
        modifyMaxSuccess (const 10) $ prop "getKey Random keys" 
            prop_gk1
            prop_gk2

-- Property test 
prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 

genStringsConst :: IO [String]
genStringsConst = do
  strs <- randomStrings (randomString randomASCII 10) 5
  return strs

prop_gk2 :: Property
prop_gk2 ==> monadicIO test where 
    test = do
        str_set <- run $ genStringsConst
        traceShow ("gk2: str_set ", str_set) $ assert $ Prelude.map (DE.isLeft str_set) == [True, True, True, True, True]
