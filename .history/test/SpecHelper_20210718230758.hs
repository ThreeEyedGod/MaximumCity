{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)
import Helper
import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Process
import System.Environment
import Data.Either as DE
import Data.Either.Combinators as DEC
import Test.RandomStrings
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

libH :: Spec
libH = describe "getkey when getting non-existent keys is always Left -> " $ do
    --libHBasic
    libHProperty1
    libHProperty2
  

libHProperty1 :: Spec
libHProperty1 = do
        modifyMaxSuccess (const 10) $ prop "getKey non-existent keys is always Left -1 " 
            prop_gk1
            prop_gk2

libHProperty2 :: Spec
libHProperty2 = do
        modifyMaxSuccess (const 10) $ prop "getKey non-existent keys is always Left -2 "
            prop_thereAndBackAgain

-- Property test 
prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 

prop_gk2 :: Property
prop_gk2 = 
    forAll genKeys $ ( \key ->
        traceShow ("gk2: forkey key ", key) $ assert (DE.isLeft $ getKey key))

genKeys :: Gen String
genKeys =  (arbitrary :: Gen String) `suchThat` (\s -> not $ Prelude.null s)

getAllEnv :: IO [(String, String)]
getEnv = getEnvironment

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll getEnv $ (\(key, val) -> 
        traceShow ("prop_thereAndBackAgain ", key, val) $ (DE.fromRight "test" (getKey key)) == val)
