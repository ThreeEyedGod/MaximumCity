{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldNotReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)
import Helper
import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Process
import System.Exit
import System.Environment as SE
import Data.Either as DE
import Data.Either.Combinators as DEC
import Test.RandomStrings
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.Hspec.Expectations (shouldReturn)
import Data.List as DL

libH :: Spec
libH = describe "getkey when getting non-existent keys is always Left -> " $ do
    libHProperty1
    libHProperty2
  
libHProperty1 :: Spec
libHProperty1 = do
        modifyMaxSuccess (const 10) $ prop "getKey non-existent keys is always Left -1 " 
            prop_gk1

libHProperty2 :: Spec
libHProperty2 = do
  modifyMaxSuccess (const 1) $ prop "set new env Key-and-get-back " $ do
        prop_gk1

prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 
propIO :: Show a => Gen a -> (a -> IO Bool) -> Property
propIO gen prop = monadicIO $ do
v <- pick gen
b <- run $ prop v
assert b


prop_thereAndBackAgain :: (String, String) -> Property
prop_thereAndBackAgain (k,v) = monadicIO $ do
              run $ SE.setEnv k v
              z <- run $ getKey k
              zplus <- run $ SE.unsetEnv k
              noErr <- run $ handle z
              assert $ (noErr == v) 

existingKeys :: IO [String]
existingKeys = do 
  x <- SE.getEnvironment
  return $ Prelude.map fst x

validKey k = (not . Prelude.null) k && '\NUL' `Prelude.notElem` k && '=' `Prelude.notElem` k 
validValue v = (not . Prelude.null) v && '\NUL' `Prelude.notElem` v
viable_newKey :: String -> IO Bool
viable_newKey k = do 
  x <- existingKeys 
  return $ not (k `DL.elem` x)

genNewKeys :: [String] -> Gen (String, String)
genNewKeys xs =  ( arbitrary `suchThat` (\(k,v) -> (validKey k) && (k `DL.notElem` xs) && (validValue v)))

genKeys :: IO (Gen (String, String))
genKeys = do
              avoidSet <- existingKeys
              return $ genNewKeys avoidSet

handle :: Either a b -> IO b
handle (Left _)  = Prelude.putStrLn "exception!" >> exitWith (ExitFailure 1)
handle (Right x) = return x