module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH


prop_top = monadicIO $ do
            keyvalpair <- run $ genKeys
            monadicIO $ do 
                run $ (forAll keyvalpair prop_thereAndBackAgain)
