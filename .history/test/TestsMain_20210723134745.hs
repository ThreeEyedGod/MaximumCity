module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH

keyvalpair <- genKeys
quickCheck . verbose $ 
            in forAll keyvalpair prop_thereAndBackAgain)
