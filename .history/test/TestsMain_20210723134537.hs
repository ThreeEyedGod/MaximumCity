module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH


quickCheck . verbose $ 
            let keyvalpair = run $ genKeys
            monadicIO $ do 
                run $ (forAll keyvalpair prop_thereAndBackAgain)
