{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- above is what allows the checking of all properties starting with prop_

module SpecHelper where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck
import Helper
import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Process
import Data.Either
import Test.QuickCheck.Monadic (assert, monadicIO, run)

libH :: Spec
libH = describe "LibH" $ do
    libHBasic
    --libHAdvanced
    libHProperty
  
libHBasic :: Spec
libHBasic = describe "LibHBasic" $ do        
    describe "getKey " $ do
      it "returns Right for colorterm" $ (Right $ getKey "COLORTERM") `shouldBe` "truecolor"

libHProperty :: Spec
libHProperty = do
        modifyMaxSuccess (const 10) $ prop "getKey Random missing keys" 
            prop_1

-- Property test 
prop_1 :: String -> Property
prop_1 s = not (null s) ==> monadicIO $ do
    x <- run $ getKey s
    return $ x
    assert $ pure $ Left $ x == MissingEnvError s