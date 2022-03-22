{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}


module InterfaceAdapters.Parameters.KVSAWSSSMParmStore
  ( runKvsAsAWSSSMParmStore
  ) 
where

import           Control.Exception
import           Data.Aeson        (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile, decode)
import           Data.List         (isSuffixOf)
import           InterfaceAdapters.Parameters.KVS (KVS (..))
import           Polysemy
import           System.Directory  (doesFileExist, listDirectory, removeFile)
import           Data.Text 

import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

import           InterfaceAdapters.Utils.Helper
import           InterfaceAdapters.Parameters.AWSSSMParmStore (doGetParameter, doPutParameter, ParameterName (..), ParameterValue (..), ssmService)
import           InterfaceAdapters.Parameters.AWSViaHaskell
import           Network.AWS.Auth
import           Network.AWS.S3.Types
import           Language.Haskell.TH
import           Network.AWS (Service)
import           Control.Lens


-- | File Based implementation of key value store
runKvsAsAWSSSMParmStore :: (Member (Embed IO) r, Show k, Read k, ToJSON v, FromJSON v, Show v) => Sem (KVS k v : r) a -> Sem r a
runKvsAsAWSSSMParmStore = interpret $ \case
  --ListAllKvs        -> embed retrieveAll
  GetKvs key        -> embed (getAction key)
  InsertKvs key val -> embed (storeEntity (show key) val)
  --DeleteKvs key     -> embed (removeFile (show key))

getAction :: (Show k, Show v, FromJSON v) => k -> IO (Maybe v)
getAction key = do
  let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ Discover
  ssmSession <- connect conf ssmService
  (value, version) <- doGetParameter (ParameterName "/AAA/BBB") ssmSession
  if version > 0 
    then do
      return (decode $ (BL.fromChunks . return . T.encodeUtf8 $ value))
    else return Nothing


-- | store persistent entity of type a and identified by id to the filesystem
storeEntity :: (ToJSON a) => String -> a -> IO ()
storeEntity key val = do 
  let conf = awsConfig (AWSRegion Mumbai) & awscCredentials .~ Discover
  ssmSession <- connect conf ssmService
  result1 <- doPutParameter (ParameterName "/AAA/BBB") (ParameterValue "CCC") ssmSession
  return ()

