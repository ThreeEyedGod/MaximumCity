module Main where
import Test.Hspec (hspec)
import qualified Spec as MT
import qualified SpecE as MTE

main :: IO ()
main = do
  hspec $ do
     MT.lib 
     MTE.spec
