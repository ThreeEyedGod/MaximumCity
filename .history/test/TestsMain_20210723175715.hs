module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.QuickCheck (listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)

import qualified SpecHelper as SH

main :: IO ()
main = do 
  hspec $ do
    SH.libH
    quickcheck $ SH.propIO SH.genKeys prop_thereAndBackAgain_bool