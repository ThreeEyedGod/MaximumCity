module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  let g = do 
    keyvalpair <- genKeys
    return 
  in quickCheck . verbose $ forAll keyvalpair prop_thereAndBackAgain)

  hspec $ do
     SH.libH

