module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as MT
import qualified SpecE as MTE

main :: IO ()
main = do
  hspec $ do
     MT.lib 
     MTE.spec
