{-|
Module      : Main
Description : Imports module for SSM demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module InterfaceAdapters.Parameters.SSMImports
    ( 
--        ParameterType(..)
      GetParameter (..)
    , GetParameterResponse (..)
    , GetParameters (..)
    , newGetParameters
    , newGetParameter
    , newGetParameterResponse
    , PutParameter (..)  
    , newPutParameter
    , getParameters_names
    , getParameter_name
    , putParameter_overwrite
    , getParameterResponse_parameter
    , getParametersResponse_parameters
    , Parameter (..)
    , newParameter
    , parameter_value
    , parameter_version 
    ) where

import Amazonka.SSM
import Amazonka.SSM.Lens (getParameter_name, putParameter_overwrite, getParameters_names, getParameterResponse_parameter, getParametersResponse_parameters)
import Amazonka.SSM.Types.Parameter (Parameter (..), newParameter, parameter_value, parameter_version )