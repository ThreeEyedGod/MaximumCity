{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (Property, arbitrary, quickCheck, (==>))
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
        modifyMaxSuccess (const 10) $ prop "getKey Random missing keys" 
            prop_gk1
            prop_gk2

-- Property test 
prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 

genStrings :: IO String
genStrings = do
  word <- randomWord randomASCII 10
  return word

prop_gk2 :: Property
prop_gk2 = do
  y <- genStrings
  traceShow ("gk2: y ", y) $ forAll y (DE.isLeft (getKey y) == True)
