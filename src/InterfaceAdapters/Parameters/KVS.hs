{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module InterfaceAdapters.Parameters.KVS 
  ( KVS (..)
  , listAllKvs
  , getKvs
  , insertKvs
  , deleteKvs
  )
where

import Polysemy
import Data.Text (Text)
import InterfaceAdapters.Parameters.Types

-- | a key value store specified as A GADT type
data KVS k v m a where
  ListAllKvs :: KVS ParameterName ParameterValue m [(ParameterName, ParameterValue)]
  GetKvs     :: ParameterName -> KVS ParameterName ParameterValue m Text
  InsertKvs  :: ParameterName -> ParameterValue -> KVS ParameterName ParameterValue m ()
  DeleteKvs  :: ParameterName -> KVS ParameterName ParameterValue m ()

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- listAllKvs :: Member (KVS k v) r => Sem r [(k, v)]
-- getKvs     :: Member (KVS k v) r => k -> Sem r (Maybe v)
-- insertKvs  :: Member (KVS k v) r => k -> v -> Sem r ()
-- deleteKvs  :: Member (KVS k v) r => k -> Sem r ()
makeSem ''KVS

