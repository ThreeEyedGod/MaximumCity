{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where

{- import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Document
 -}
import qualified Data.Text as T
import ByteString
import Prelude

wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"
getDocatThisURL ::  IO ByteString
getDocatThisURL = simpleHttp wlURL `X.catch` statusExceptionHandler

type Lakename = T.Text

getLakeLevelFor :: Lakename -> IO T.Text 
getLakeLevelat ln = do
  withPdfFile wlURL $ \pdf -> 



    pure txt
 