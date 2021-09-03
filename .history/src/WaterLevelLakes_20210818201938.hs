{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where

import Pdf.Document
import JSONHelper

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString (ByteString)
import Control.Exception as X

import Network.HTTP.Conduit (simpleHttp)

import Prelude

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.
import qualified Data.ByteString as B


wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"
getPdfAtThisURL ::  IO B.ByteString
getPdfAtThisURL = simpleHttp wlURL `X.catch` exceptionHandler

type Lakename = T.Text

getLakeLevelFor :: Lakename -> IO T.Text 
getLakeLevelFor lName = do
      pdfByteString <- getPdfAtThisURL
      pdf <- fromBytes $ B.toStrict pdfByteString
      doc <- document pdf

      maybe_info <- documentInfo doc
      title <- case maybe_info of
          Nothing -> return Nothing
          Just info -> infoTitle info

      root <- documentCatalog doc >>= catalogPageNode
      total <- pageNodeNKids root

      return (root, total, title)

      firstPage <- pageNodePageByNum root 0
      eigthPage <- pageNodePageByNum root 7 
      txt <- pageExtractText eigthPage

      pure txt
 
data LakeEntry = Lakename {
    srNo :: Int,
    revenueRegion :: Text,
    noDams :: Int,
    

}
 