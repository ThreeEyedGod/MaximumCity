module Main where
import Test.Hspec (hspec)
import qualified SpecHelper as SH
--import qualified SpecHelperE as SHE

main :: IO ()
main = do
  hspec $ do
     SH.libH
     --SHE.spec
