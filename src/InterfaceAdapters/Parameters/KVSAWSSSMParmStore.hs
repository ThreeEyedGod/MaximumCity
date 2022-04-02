{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module InterfaceAdapters.Parameters.KVSAWSSSMParmStore
  ( runKvsAsAWSSSMParmStore
  ) 
where

import           Control.Exception
import           Data.Aeson        (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile, decode, decode', decodeStrict', Value)
import           Data.List         (isSuffixOf)
import           InterfaceAdapters.Parameters.KVS (KVS (..))
import           Polysemy
import           System.Directory  (doesFileExist, listDirectory, removeFile)
import           Data.Text 
import           qualified Data.List.NonEmpty as NonEmpty (fromList)

import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

import           InterfaceAdapters.Utils.Helper
import           InterfaceAdapters.Parameters.AWSSSMParmStore (doGetParameter, doGetParameterArr, doPutParameter, ParameterName (..), ParameterValue (..), ssmService)
import           InterfaceAdapters.Parameters.AWSViaHaskell
import           Network.AWS.Auth
import           Network.AWS.S3.Types
import           Language.Haskell.TH
import           Network.AWS (Service)
import           Control.Lens

-- | File Based implementation of key value store
runKvsAsAWSSSMParmStore :: (Member (Embed IO) r, Show k, ToJSON v, FromJSON v) => Sem (KVS k v : r) a -> Sem r a
runKvsAsAWSSSMParmStore = interpret $ \case
  --ListAllKvs        -> embed retrieveAll
  GetKvs key        -> embed (getAction key)
  -- GetKvs key        -> embed (getAction1 key)
  InsertKvs key val -> embed (storeEntity (show key) val)
  --DeleteKvs key     -> embed (removeFile (show key))

getAction :: (Show k, FromJSON v) => k -> IO (Maybe v)
getAction key = do
  let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ Discover
  ssmSession <- connect conf ssmService
  (valueText, version) <- doGetParameter (ParameterName "/AAA/BBB") ssmSession 
  logMessage $ T.unpack valueText
  let valueBS = T.encodeUtf8 valueText
  let vMaybe = decodeStrict' valueBS 
  return $ vMaybe

{- getAction1 :: (Show k, FromJSON v) => k -> IO (Maybe v)
getAction1 key = do
  let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ Discover
  ssmSession <- connect conf ssmService
  rows <- doGetParameterArr (ParameterName "/AAA/BBB") ssmSession :: IO [ParameterValue]
  case rows of
    [] -> return Nothing
    value : _ -> return $ decodeStrict' $ T.encodeUtf8 value 
 -}

storeEntity :: (ToJSON a) => String -> a -> IO ()
storeEntity key val = do
  let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ Discover
  ssmSession <- connect conf ssmService
  result1 <- doPutParameter (ParameterName "/AAA/BBB") (ParameterValue (T.pack $ "{\"value\" : \"CCC\"}")) ssmSession
  return ()
