{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelHeaders where

import Pdf.Document
import JSONHelper

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString (ByteString)
import Prelude

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative

type Lakename = T.Text

type ColHeader = T.Text
type ColumnNos = Int
type ProjectHeader = T.Text

data Page8Page9 = Page8Page9 {
    statusDate :: T.Text,
    topicTitle :: T.Text,
    revenueRegionHeader :: T.Text,
    colHeader :: RevenueRegionHeader,
    colNumbers :: ColumnNos,
    majorMaharashtraStateProjects :: CategoryProjects,
    mediumMaharashtraStateProjects :: CategoryProjects,
    minorMaharashtraStateProjects :: CategoryProjects,
    allDamsMaharashtraStateProjects :: CategoryProjects
} deriving Show

data CategoryProjects = CategoryProjects {
    categoryHeader :: ProjectHeader,
    categoryProjects :: [RegionEntry],
    totalCategoryProjects :: TotalProjectFooter
} deriving Show

data ProjectHeader = ProjectHeader {
    projectsHeader :: T.Text
} deriving Show

data TotalProjectFooter = TotalProjectFooter {
    projectsFooter :: T.Text,
    waterDataTotal :: RegionWaterData
} deriving Show

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
} deriving Show

data RegionEntry = RegionEntry {
    srNo :: Int,
    revenueRegion :: T.Text,
    noDams :: Int,
    waterData :: RegionWaterData
} deriving Show

data RegionWaterData = RegionWaterData {
    dead_DesignedStorage :: Float,
    live_DesignedStorage :: Float,
    gross_DesignedStorage :: Float,
    live_TodayLiveStorage :: Float,
    gross_TodayLiveStorage :: Float,
    percent_LiveStorage_WRT_liveDesignedStorage :: T.Text,
    percent_LiveStorage_WRT_sameDateLastYear :: T.Text
} deriving Show