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
libH = describe "getkey Tests" $ do
    libHProperty1
    libHProperty2
    libHProperty3

libHProperty1 :: Spec
libHProperty1 = do
        modifyMaxSuccess (const 10) $ prop "Test: Always isLeft for missing" 
            prop_gk1

libHProperty2 :: Spec
libHProperty2 = do
        modifyMaxSuccess (const 10) $ prop "Test: there and back again " 
            prop_thereAndBackAgain

libHProperty3 :: Spec
libHProperty3 = do
        modifyMaxSuccess (const 10) $ prop "Test: check existing values "
            prop_checkExistingKeys

-- in general random strings should not exist as keys and should result from getkey as Left values
-- there is a slight chance of error here
prop_gk1 :: String -> Property
prop_gk1 forkey = not (Prelude.null forkey) ==> monadicIO test where 
    test = do 
        x <- run $ getKey forkey
        traceShow ("gk1: forkey x ", (forkey, x)) $ assert (DE.isLeft x) 

-- Because genExistingKeys are IO, it has to be monadicIO 
prop_checkNonExistentKeys :: Property
prop_checkNonExistentKeys = monadicIO $ do
              i <- run $ genKeys -- IO hence needs run $. genKeys returns brand new unset keys
              j <- pick i -- not an IO
              -- do not set this key !
              z <- run $ getKey (fst j) -- IO attempt to get new key value using our function
              -- no need to unset anything. We have not set a anything !
              shouldBeErr <- run $ handle z -- IO extract from our getKey. Hopefully Left something 
              assert $ (shouldBeErr == snd j)  -- compare


-- Turns an IO gotten value and a function of type a -> IO a into a regular property.
-- Note: Bind (<-) operations must be run $ SomeIOFunction. Others may be bound without using run
-- monadicIO properties must end with assert in general
propIO :: Show a => IO (Gen a) -> (a -> IO Bool) -> Property
propIO gen prop = monadicIO $ do
  i <- run $ gen
  v <- pick i
  b <- run $ prop v
  assert b

-- Turns a value and a function of type a -> IO a into a regular property.
propIO2 :: Show a => Gen a -> (a -> IO Bool) -> Property
propIO2 gen prop = monadicIO $ do
  v <- pick gen
  b <- run $ prop v
  assert b

-- input may not be IO but because setEnv and unsetEnv are IO, o/p is marked as IO Bool
prop_thereAndBackAgain_bool :: (String, String) -> IO Bool
prop_thereAndBackAgain_bool (k,v) = do 
      SE.setEnv k v
      z <- getKey k
      cleanSetVals <- SE.unsetEnv k
      noErr <- handle z
      return $ noErr == v

-- input may not be IO but because setEnv and unsetEnv are IO, it is a monadicIO 
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = monadicIO $ do
              i <- run $ genKeys -- IO hence needs run $. genKeys returns brand new unset keys
              j <- pick i -- not an IO
              run $ SE.setEnv (fst j) (snd j) -- IO a pair-value hence fst and snd
              z <- run $ getKey (fst j) -- IO get new key value using our function
              cleanSetVals <- run $ SE.unsetEnv (fst j) -- IO remove what we set. Teardown. 
              noErr <- run $ handle z -- IO extract from our getKey 
              assert $ (noErr == MissingEnvError env)  -- compare

-- Because genExistingKeys are IO, it has to be monadicIO 
prop_checkExistingKeys :: Property
prop_checkExistingKeys = monadicIO $ do
              i <- run $ genExistingKeys
              j <- pick i
              z <- run $ getKey (fst j) -- IO: get key value using our function
              noErr <- run $ handle z -- extract our value
              assert $ (noErr == snd j)  -- compare with existing value

-- an getEnvironment operation results in a list of key strings currently in the environment. Hence marked 
-- as IO String
existingKeys :: IO [String]
existingKeys = do 
  x <- SE.getEnvironment
  return $ Prelude.map fst x

existingKeyValuePairs :: IO [(String, String)]
existingKeyValuePairs = do 
  x <- SE.getEnvironment
  return $ x

genExistingKeys :: IO (Gen (String, String))
genExistingKeys = do
  x <- SE.getEnvironment
  return $ elements x 

validKey k = (not . Prelude.null) k && '\NUL' `Prelude.notElem` k && '=' `Prelude.notElem` k 
validValue v = (not . Prelude.null) v && '\NUL' `Prelude.notElem` v
-- viable new keys for enviroment are those which are not existing
viable_newKey :: String -> IO Bool
viable_newKey k = do 
  x <- existingKeys 
  return $ not (k `DL.elem` x)

-- Given an inout list of strings, generate new strings but avoid strings from the givern list
genNewKeys :: [String] -> Gen (String, String)
genNewKeys xs =  ( arbitrary `suchThat` (\(k,v) -> (validKey k) && (k `DL.notElem` xs) && (validValue v)))

-- get existing environment keys and avoid making those when making new keys
genKeys :: IO (Gen (String, String))
genKeys = do
              avoidSet <- existingKeys
              return $ genNewKeys avoidSet

-- Given an Either-or input, extract b out of it, if it was Right b else exit with failure
handle :: Either a b -> IO b
--handle (Left _)  = Prelude.putStrLn "exception!" >> exitWith (ExitFailure 1)
handle (Left err)  = return err
handle (Right x) = return x

badEnv env ex
      | Prelude.null env = return $ Left $ EmptyKeyError "Environment key not given "
      | isDoesNotExistError ex = return $ Left $ (MissingEnvError env)
      | otherwise = return $ Left $ SomeIOError ex
