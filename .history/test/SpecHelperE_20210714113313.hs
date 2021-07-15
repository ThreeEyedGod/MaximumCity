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
    prop_square

prop_square :: Property
prop_square = forAll genStrings (\x -> traceShow ("numbers: ", (x, karatsuba x x)) $ karatsuba x x == x * x)
