{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-@ LIQUID "--skip-module" @-}

module InterfaceAdapters.Parameters.AWSSSMParmStore
  ( doGetParameter,
    doPutParameter,
    ParameterName (..),
    ParameterValue (..),
  )
where

import Amazonka
import qualified Amazonka as AWS
import Control.Lens
import Control.Monad (void)
import Data.Aeson.Encoding (text)
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.Monoid ((<>))
import Data.Text (Text, null)
import qualified Data.Text.IO as Text (putStrLn)
import GHC.Generics
import InterfaceAdapters.Parameters.SSMImports
import InterfaceAdapters.Parameters.Types
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import qualified System.IO as IO
import Amazonka.SSM.Types.ParameterType
-- see https://github.com/ucsd-progsys/liquidhaskell/issues/1094
-- import Language.Haskell.Liquid.ProofCombinators

awsEnvIdentity :: IO (Env' Identity) 
awsEnvIdentity =   do 
  logger <- AWS.newLogger AWS.Debug IO.stdout
  discoveredEnv <- AWS.newEnv AWS.discover
  let env =
        discoveredEnv
          { AWS.envLogger = logger,
            AWS.envRegion = AWS.Mumbai
          }
  return env

doGetParameter :: ParameterName -> IO Text
doGetParameter pn = do 
  env <- awsEnvIdentity
  AWS.runResourceT $ do
    result <- AWS.send env $ newGetParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. getParametersResponse_parameters
    let pVal = param ^. parameter_value
    -- return $ (pVal, param ^. parameter_version)
    return pVal

doPutParameter :: ParameterName -> ParameterValue -> IO ()
doPutParameter pn pv = do
  env <- awsEnvIdentity
  AWS.runResourceT $ do
    void (AWS.send env $ newPutParameter pn pv & (putParameter_type .~ Just ParameterType_String) & (putParameter_dataType .~ (Just "text")) & (putParameter_overwrite .~ Just True))

