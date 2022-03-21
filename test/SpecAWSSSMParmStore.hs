{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SpecAWSSSMParmStore where
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldNotReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Debug.Trace
import Test.QuickCheck (expectFailure, listOf, suchThat, elements, Arbitrary, Property, arbitrary, quickCheck, (==>), forAll, Gen, choose)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, forAllM)
import Test.Hspec.Expectations (shouldReturn)
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           Control.Monad (void)
import           Control.Lens
import Network.AWS.Auth
import Network.AWS.S3.Types
import           Language.Haskell.TH
import           Network.AWS (Service)

import Control.Exception as X
import Control.Exception (IOException, handle, throw, catch)
import System.IO.Error (isDoesNotExistError, tryIOError)

import InterfaceAdapters.Parameters.AWSSSMParmStore
import InterfaceAdapters.Parameters.AWSViaHaskell

libAWSSSM :: Spec
libAWSSSM = describe "AWS SSM Parm Store Tests" $ do
    libAWSSSMParmStore1

libAWSSSMParmStore1 :: Spec
libAWSSSMParmStore1 = 
  describe "AWS SSM Parm Store Set Parm and Get Parm -34" $ do
      it "sets a value for a junky new key -23 " $ do 
{-         homeDir <- getHomeDirectory
        let conf = awsConfig (AWSRegion Mumbai)
                    & awscCredentials .~ (FromFile "MaximumCity" $ homeDir </> ".aws" </> "credentials")
      -}
{-         let conf = awsConfig (AWSRegion Mumbai)
                    & awscCredentials .~ Discover   
 -}        
        let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ FromProfile "MaximumCity-role-3x5dxzgh"
        ssmSession <- connect conf ssmService

        putStrLn "doPutParameter"
        result1 <- doPutParameter (ParameterName "/AAA/BBB") (ParameterValue "CCC") ssmSession
        result1 `shouldBe` ()

      it "gets a value for a existent key -22 " $ do
{-         homeDir <- getHomeDirectory
        let conf = awsConfig (AWSRegion Mumbai)
                    & awscCredentials .~ (awscCredentials "MaximumCity" $ homeDir </> ".aws" </> "credentials")
 -}      
{-         let conf = awsConfig (AWSRegion Mumbai)
                    & awscCredentials .~ Discover   
 -}        
        let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ FromProfile "MaximumCity-role-3x5dxzgh"
        ssmSession <- connect conf ssmService

        putStrLn "doGetParameter"
        (value, version) <- doGetParameter (ParameterName "/AAA/BBB") ssmSession
        putStrLn $ "Value: " <> show value
        putStrLn $ "Version: " ++ show version
        (value, version) `shouldBe` ("CCC", 1)
