{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module InterfaceAdapters.Parameters.AWSSSMParmStore
    ( 
--        ParameterType(..)
--    , getParameters
{-     , grsParameters
    , pValue
    , pVersion
    , ppOverwrite -}
--    , putParameter
--    , ssm
      doGetParameter
   -- , doGetParameterArr
    , doPutParameter
    , ParameterName (..)
    , ParameterValue (..)
--    , SSMSession
--    , ssmService
    ) where


-- import Network.AWS
import Amazonka
import qualified Amazonka as AWS
import qualified System.IO as IO

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Aeson.Types( Value(..) )
--import           InterfaceAdapters.Parameters.AWSViaHaskell
import           Control.Monad (void)
import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           InterfaceAdapters.Parameters.SSMImports
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

import GHC.Generics
-- import Text.JSON.Generic

import Polysemy
import Polysemy.State

-- wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text
newtype ParameterValue = ParameterValue Text deriving (FromJSON)

{- doGetParameter :: ParameterName -> SSMSession -> IO (Text, Integer)
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. grsParameters
    let pVal = fromJust (param ^. pValue)
    return $ (pVal, fromJust (param ^. pVersion))

doPutParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = withAWS $
    void (send $ putParameter pn pv String & ppOverwrite .~ Just True)
 -}


doGetParameter :: ParameterName -> IO (Text, Integer)
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
        return $ (pVal, param ^. parameter_version)

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
        void (AWS.send env $ newPutParameter pn pv & putParameter_overwrite .~ Just True) 

{- doGetParameterArr :: ParameterName -> SSMSession -> IO [Text]
doGetParameterArr (ParameterName pn) = withAWS $ do
  result <- send $ getParameters (NonEmpty.fromList [pn])
  let paramArr = result ^. grsParameters
  return $ map (\x -> fromJust (x ^. pValue)) paramArr
 -}