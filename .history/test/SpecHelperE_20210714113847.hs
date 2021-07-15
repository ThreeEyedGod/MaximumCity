module SpecE where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import Debug.Trace
import Helper

-- why does chooseinteger not work?
genStrings :: Gen String
genStrings = choose ("ABCDE" , "EFGHI")

spec :: Spec
spec = 
  prop "getKey with generated strings "  
    prop_getkey

prop_getkey :: Property
prop_getkey = forAll genStrings (\x -> traceShow ("strings: ", (x, getKey x)) $ getKey x == x)
