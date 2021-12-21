{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module SpecEnvPolHelper where
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldNotReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (expectFailure, listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)

import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.Hspec.Expectations (shouldReturn)

import InterfaceAdapters.Utils.EnvPolHelper

libEPH :: Spec
libEPH = describe "getkey Env Polysemy Tests" $ do
    libEPH1

libEPH1 :: Spec
libEPH1 = 
  describe "Polysemy based get Environment Key" $ do

      it "gets a value for a junky non-existent key " $ do 
        result1 <- runGetKey "abracadabra"
        result1 `shouldBe` Nothing

      it "gets a value for a existent key " $ do
        result2 <- runGetKey "HOSTNAME" 
        result2 `shouldBe` Just "docker-desktop"



