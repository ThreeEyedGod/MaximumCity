module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)

import qualified SpecHelper as SH

main :: IO ()
main = monadicIO $ do 
    kvpair <- run $ genKeys
    return $ prop_thereAndBackAgain kvpair
  in quickCheck . verbose $ forAll g id

  hspec $ do
     SH.libH

