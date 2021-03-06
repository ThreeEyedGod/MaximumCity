{-# LANGUAGE GADTs, TypeInType, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

-- | Effectful implmentation of Environment getting functions. Not yet used
module InterfaceAdapters.Utils.EnvPolHelper where

import Polysemy
import System.Environment
import Data.Function             ((&))

data Configuration m a where
  ReadConf :: String -> Configuration m (Maybe String)

makeSem ''Configuration

runconfToIO :: Member (Embed IO) r => Sem (Configuration ': r) a -> Sem r a
runconfToIO = interpret (\(ReadConf envVarName) -> embed $ lookupEnv envVarName)

runGetKey :: String -> IO (Maybe String)
runGetKey env = do 
  readConf env
  & runconfToIO
  & runM

