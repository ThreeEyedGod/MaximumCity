module SpecE where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import Debug.Trace
import Lib

-- why does chooseinteger not work?
genIntegers :: Gen Integer
genIntegers = choose (1000000000 , 1000000090)

spec :: Spec
spec = 
  prop "Karatsuba with large generated numbers-2 "  
    prop_square

prop_square :: Property
prop_square = forAll genIntegers (\x -> traceShow ("numbers: ", (x, karatsuba x x)) $ karatsuba x x == x * x)
