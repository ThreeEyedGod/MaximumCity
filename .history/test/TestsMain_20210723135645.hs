module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  let g = do 
    keyvalpair <- genKeys
    return prop_thereAndBackAgain
  in quickCheck . verbose $ forAll g )

  hspec $ do
     SH.libH
