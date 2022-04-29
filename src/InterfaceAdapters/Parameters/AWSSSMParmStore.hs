{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module InterfaceAdapters.Parameters.AWSSSMParmStore
    ( 
      doGetParameter
    , doPutParameter
    , ParameterName (..)
    , ParameterValue (..)
    ) where

import Amazonka
import qualified Amazonka as AWS
import qualified System.IO as IO

import           Control.Monad (void)
import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import GHC.Generics
import InterfaceAdapters.Parameters.Types
import InterfaceAdapters.Parameters.SSMImports
import Data.Aeson.Encoding (text)

doGetParameter :: ParameterName -> IO Text 
doGetParameter (ParameterName pn) = do 
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discoveredEnv <- AWS.newEnv AWS.discover
    let env =
          discoveredEnv
                { AWS.envLogger = logger
                , AWS.envRegion = AWS.Mumbai
                }
    AWS.runResourceT $ do 
        result <- AWS.send env $ newGetParameters (NonEmpty.fromList [pn])
        let param = head $ result ^. getParametersResponse_parameters
        let pVal = param ^. parameter_value
        -- return $ (pVal, param ^. parameter_version)
        return pVal

doPutParameter :: ParameterName -> ParameterValue -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = do
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discoveredEnv <- AWS.newEnv AWS.discover
    let env =
          discoveredEnv
                { AWS.envLogger = logger
                , AWS.envRegion = AWS.Mumbai
                }
    AWS.runResourceT $ do
        void (AWS.send env $ newPutParameter pn pv & (putParameter_dataType  .~ (Just "text")) & (putParameter_overwrite .~ Just True))
