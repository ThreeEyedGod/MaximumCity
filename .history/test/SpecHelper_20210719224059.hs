{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)
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
import Test.Hspec.Expectations (shouldReturn)
--import Test.QuickCheck.IO ()

libH :: Spec
libH = describe "getkey when getting non-existent keys is always Left -> " $ do
    libHProperty1
    libHProperty2
  
libHProperty1 :: Spec
libHProperty1 = do
        modifyMaxSuccess (const 10) $ prop "getKey non-existent keys is always Left -1 " 
            prop_gk1


libHProperty2 :: Spec
libHProperty2 = do
  modifyMaxSuccess (const 10) $
    prop "set env and back from getKey "
      prop_setAndBackAgain


prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 

genKeys :: Gen String
genKeys =  (arbitrary :: Gen String) `suchThat` (\s -> not $ Prelude.null s)

genListofKeys :: Gen [String]
genListofKeys = listOf genKeys

prop_setAndBackAgain k v =
  validKey k && validValue v ==> do
    setEnv k v
    getKey k `shouldReturn` (Right $ v) 

validKey k = (not . Prelude.null) k && '\NUL' `Prelude.notElem` k && '=' `Prelude.notElem` k
validValue v = (not . Prelude.null) v && '\NUL' `notElem` v