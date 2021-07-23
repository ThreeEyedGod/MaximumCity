module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH


quickCheck . verbose $ 
            let keyvalpair = genKeys
            in
                run $ (forAll keyvalpair prop_thereAndBackAgain)