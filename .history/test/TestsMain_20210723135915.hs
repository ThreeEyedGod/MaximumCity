module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)

import qualified SpecHelper as SH

main :: IO ()
main = 
  let g = monadicIO $ do 
    keyvalpair <- run $ genKeys
    return prop_thereAndBackAgain
  in quickCheck . verbose $ forAll g id)

  hspec $ do
     SH.libH

