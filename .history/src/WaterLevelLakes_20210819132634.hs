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
 

data Page8 = Page8 {

    totalMaharashtraStateProjects :: TotalProjectFooter
}

data ColumnNos = ColumnNos {
    one :: Int,
    two :: Int,
    three :: Int,
    four :: Int,
    five :: Int,
    six :: Int,
    seven :: Int,
    eight :: Int,
    nine :: Int,
    ten :: Int
}

data ProjectHeader = ProjectHeader {
    projectsHeader :: T.Text
}
data TotalProjectFooter = TotalProjectFooter {
    projectsFooter :: T.Text,
    waterDataTotal :: RegionWaterData
}

data RevenueRegionHeader = RevenueRegionHeader {
    srNo :: T.Text,
    revenueRegion :: T.Text,
    noDams :: T.Text,
    designedStorage :: T.Text,
    todaysLiveStorage :: T.Text,
    percentageLiveStorageWRTDesignedLiveStorage :: T.Text,
    dead_DesignedStorage :: T.Text,
    live_DesignedStorage :: T.Text,
    gross_DesignedStorage :: T.Text,
    live_TodayLiveStorage :: T.Text,
    gross_TodayLiveStorage :: T.Text,
    percent_Today_LiveStorage :: T.Text,
    percent_SameDateLastYear_LiveStorage:: T.Text
}

data RegionEntry = RegionEntry {
    srNo :: Int,
    revenueRegion :: T.Text,
    noDams :: Int,
    waterData :: RegionWaterData
}

data RegionWaterData = RegionWaterData {
    dead_DesignedStorage :: T.Text,
    live_DesignedStorage :: T.Text,
    gross_DesignedStorage :: T.Text,
    live_TodayLiveStorage :: T.Text,
    gross_TodayLiveStorage :: T.Text,
    percent_LiveStorage_WRT_liveDesignedStorage :: T.Text,
    percent_LiveStorage_WRT_sameDateLastYear :: T.Text
}