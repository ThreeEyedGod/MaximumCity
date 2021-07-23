module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH

main :: IO ()
main = do
  hspec $ do
     SH.libH

  hspec $ do 
    describe "goThereBackAgain" $ do 
     prop "it can do that " $ 
       prop_

