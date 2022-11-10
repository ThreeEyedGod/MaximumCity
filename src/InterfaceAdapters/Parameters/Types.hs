module InterfaceAdapters.Parameters.Types where

import Data.ByteString                  (ByteString)
import Data.String                      (IsString)
import Data.Text (Text)
import Data.Data (Typeable)

type ParameterName = Text
type ParameterValue = Text