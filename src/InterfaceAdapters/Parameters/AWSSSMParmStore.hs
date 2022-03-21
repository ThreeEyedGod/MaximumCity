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
    , doPutParameter
    , ParameterName (..)
    , ParameterValue (..)
    , SSMSession
    , ssmService
    ) where


import Network.AWS


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

import Polysemy
import Polysemy.State


wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text

newtype ParameterValue = ParameterValue Text

doGetParameter :: ParameterName -> SSMSession -> IO (Text, Integer)
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. grsParameters
    return $ (fromJust (param ^. pValue), fromJust (param ^. pVersion))

doPutParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = withAWS $
    void (send $ putParameter pn pv String & ppOverwrite .~ Just True)
