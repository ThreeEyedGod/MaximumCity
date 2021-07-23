module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)

import qualified SpecHelper as SH

main :: IO ()
main = monadicIO $ do 
    kvpair <- run $ genKeys
    quickCheck . verbose $ 
      forAll kvpair $ \kvp 
          prop_thereAndBackAgain kvp
      
  --hspec $ do
    -- SH.libH