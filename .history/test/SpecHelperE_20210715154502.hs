module SpecHelperE where

import Test.Hspec
import Test.QuickCheck
import Test.RandomStrings
import Test.Hspec.QuickCheck (prop)
import Debug.Trace
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Data.Either

import Helper

genStrings :: IO String
genStrings = do 
  word <- randomWord randomASCII 10
  return word

spec :: Spec
spec = 
  prop "getKey with generated strings "  
    prop_getkey

prop_getkey :: Property
prop_getkey = do 
  y <- genStrings
  forAll genStrings (\x -> traceShow ("strings: ", (x, getKey x)) $ (isLeft $ getKey x) == True)
