module SpecHelperE where

import Test.Hspec
import Test.QuickCheck
import Test.RandomStrings
import Test.Hspec.QuickCheck (prop)
import Debug.Trace
import Helper

-- why does chooseinteger not work?
genStrings :: String
genStrings = do 
  word <- randomWord' randomASCII (1%10) 10
spec :: Spec
spec = 
  prop "getKey with generated strings "  
    prop_getkey

prop_getkey :: Property
prop_getkey = forAll genStrings (\x -> traceShow ("strings: ", (x, getKey x)) $ getKey x == x)
