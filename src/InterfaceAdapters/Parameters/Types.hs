{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module InterfaceAdapters.Parameters.Types where 

import Data.ByteString                  (ByteString)
import Data.String                      (IsString)
import Data.Text (Text)


newtype ParameterName = ParameterName Text deriving (IsString, Eq, Ord, Show)
-- newtype ParameterValue = ParameterValue Text deriving (FromJSON)
newtype ParameterValue = ParameterValue Text deriving (IsString, Eq, Ord, Show)
