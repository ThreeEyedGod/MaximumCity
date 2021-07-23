module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH

let g = do 
keyvalpair <- genKeys
quickCheck . verbose $ forAll keyvalpair prop_thereAndBackAgain)