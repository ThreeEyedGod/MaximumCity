module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.QuickCheck

import qualified SpecHelper as SH

main :: IO ()
main = do 
  --works as below. But the hspec one also does. Hence returing this one
  --quickCheck . verbose $ SH.propIO SH.genKeys SH.prop_thereAndBackAgain_bool
  hspec $ do
    SH.libH
