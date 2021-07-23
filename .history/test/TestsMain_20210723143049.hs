module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.QuickCheck (listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)

import qualified SpecHelper as SH

main :: IO ()
main = monadicIO $ do 
    kvpair <- run $ SH.genKeys
    run $ quickCheck $ 
      forAll kvpair $ \kvp ->
          SH.prop_thereAndBackAgain kvp
      
  --hspec $ do
    -- SH.libH