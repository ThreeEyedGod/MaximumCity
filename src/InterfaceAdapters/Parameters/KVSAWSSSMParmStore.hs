{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module InterfaceAdapters.Parameters.KVSAWSSSMParmStore
  ( runKvsAsAWSSSMParmStore
  ) 
where

import           Control.Exception
import           Data.Aeson        (FromJSON, ToJSON, encode, decode, decode', decodeStrict', Value, eitherDecodeStrict', eitherDecodeStrict, decodeStrict)
import           Data.List         (isSuffixOf)
import           InterfaceAdapters.Parameters.KVS (KVS (..))
import           Polysemy
import           System.Directory  (doesFileExist, listDirectory, removeFile)
import           Data.Text 
import           qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.Maybe
--import Data.String.Interpolate.IsString
import Data.String.Interpolate ( i )

import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
import Data.String (IsString)
import InterfaceAdapters.Parameters.Types

import           InterfaceAdapters.Utils.Helper
import           InterfaceAdapters.Parameters.AWSSSMParmStore (doGetParameter, doPutParameter, ParameterName (..), ParameterValue (..))
import           Amazonka.Auth
import           Amazonka.S3.Types
import           Language.Haskell.TH
import           Amazonka (Service)
import           Control.Lens

import           GHC.Generics

--runKvsAsAWSSSMParmStore :: (Member (Embed IO) r, Read k, Show k, Show v, ToJSON v, FromJSON v) => Sem (KVS k v : r) a -> Sem r a
runKvsAsAWSSSMParmStore :: (Member (Embed IO) r) => Sem (KVS ParameterName ParameterValue : r) a -> Sem r a
runKvsAsAWSSSMParmStore = interpret $ \case
  --ListAllKvs        -> embed retrieveAll
  GetKvs key        -> embed $ doGetParameter (ParameterName "/AAA/BBB")
  InsertKvs key val -> embed $ doPutParameter (ParameterName "/AAA/BBB") val
  --DeleteKvs key     -> embed (removeFile (show key))