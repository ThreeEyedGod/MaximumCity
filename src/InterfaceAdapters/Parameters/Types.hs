{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module InterfaceAdapters.Parameters.Types where 

import Data.ByteString                  (ByteString)
import Data.String                      (IsString)
import Data.Text (Text)
import Data.Data (Typeable)

--newtype ParameterName = ParameterName Text deriving (IsString, Eq, Ord, Show, Typeable)
--newtype ParameterValue = ParameterValue Text deriving (IsString, Eq, Ord, Show, Typeable)

type ParameterName = Text 
type ParameterValue = Text 