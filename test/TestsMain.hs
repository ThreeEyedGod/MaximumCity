module Main where
import Test.Hspec (hspec)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.QuickCheck

import qualified SpecHelper as SH
import qualified SpecEnvPolHelper as SEPH
import System.Environment
import Debug.Trace


{- printEnvironment :: [(String, String)] -> IO ()
printEnvironment [] = pure ()
printEnvironment (elem : xs) = do 
  putStrLn $ fst elem 
  putStrLn " "
  putStrLn $ snd elem 
  putStrLn " "
  printEnvironment xs -}
  

main :: IO ()
main = do 
  
{-   a <- getEnvironment
  printEnvironment a
 -}
  hspec $ do
    SH.libH
    SEPH.libEPH
