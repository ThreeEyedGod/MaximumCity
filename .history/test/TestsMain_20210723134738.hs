module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH

keyvalpair
quickCheck . verbose $ 
            let keyvalpair = genKeys
            in forAll keyvalpair prop_thereAndBackAgain)
