{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module InterfaceAdapters.Parameters.AWSSSMParmStore
    ( ParameterType(..)
    , getParameters
    , grsParameters
    , pValue
    , pVersion
    , ppOverwrite
    , putParameter
    , ssm
    , doGetParameter
    , doGetParameterArr
    , doPutParameter
    , ParameterName (..)
    , ParameterValue (..)
    , SSMSession
    , ssmService
    ) where


import Network.AWS

import Data.Aeson.Types (FromJSON, ToJSON)
import           InterfaceAdapters.Parameters.AWSViaHaskell
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
import Text.JSON.Generic

import Polysemy
import Polysemy.State


wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text
newtype ParameterValue = ParameterValue Text deriving (FromJSON)

doGetParameter :: ParameterName -> SSMSession -> IO (Text, Integer)
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. grsParameters
    let pVal = fromJust (param ^. pValue)
    return $ (pVal, fromJust (param ^. pVersion))

doPutParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = withAWS $
    void (send $ putParameter pn pv String & ppOverwrite .~ Just True)

doGetParameterArr :: ParameterName -> SSMSession -> IO [Text]
doGetParameterArr (ParameterName pn) = withAWS $ do
  result <- send $ getParameters (NonEmpty.fromList [pn])
  let paramArr = result ^. grsParameters
  return $ map (\x -> fromJust (x ^. pValue)) paramArr
