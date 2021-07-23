module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = 
  let g = modo 
    keyvalpair <- genKeys
    return prop_thereAndBackAgain
  in quickCheck . verbose $ forAll g id)

  hspec $ do
     SH.libH

