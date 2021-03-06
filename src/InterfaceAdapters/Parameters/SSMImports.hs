module InterfaceAdapters.Parameters.SSMImports
    ( 
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
    , putParameter_dataType
    , putParameter_type
    ) where

import Amazonka.SSM
import Amazonka.SSM.Lens (getParameterResponse_parameter, getParameter_name, getParametersResponse_parameters, getParameters_names, putParameter_dataType, putParameter_overwrite, putParameter_type)
import Amazonka.SSM.Types.Parameter (Parameter (..), newParameter, parameter_value, parameter_version)
