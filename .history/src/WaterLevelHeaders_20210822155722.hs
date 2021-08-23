{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelHeaders where

import Pdf.Document

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Prelude

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative

type Lakename = DB.ByteString

type ColHeader = DB.ByteString
type ColumnNos = Int
type ProjectHeader = DB.ByteString

data Page8Page9 = Page8Page9 {
    statusDate :: DB.ByteString,
    topicTitle :: DB.ByteString,
    revenueRegionHeader :: DB.ByteString,
    colHeader :: [ColHeader],
    colNumbers :: [ColumnNos],
    majorMaharashtraStateProjects :: CategoryProjects,
    mediumMaharashtraStateProjects :: CategoryProjects,
    minorMaharashtraStateProjects :: CategoryProjects,
    allDamsMaharashtraStateProjects :: CategoryProjects
} deriving Show

data SectionHeaders = SectionHeaders {}

data CategoryProjects = CategoryProjects {
    categoryHeader :: ProjectHeader,
    categoryProjects :: [RegionEntry],
    totalCategoryProjects :: TotalProjectFooter
} deriving Show

data TotalProjectFooter = TotalProjectFooter {
    projectsFooter :: DB.ByteString,
    waterDataTotal :: RegionWaterData
} deriving Show

data RevenueRegionHeader = RevenueRegionHeader {
    srNo :: DB.ByteString,
    revenueRegion :: DB.ByteString,
    noDams :: DB.ByteString,
    designedStorage :: DB.ByteString,
    todaysLiveStorage :: DB.ByteString,
    percentageLiveStorageWRTDesignedLiveStorage :: DB.ByteString,
    dead_DesignedStorage :: DB.ByteString,
    live_DesignedStorage :: DB.ByteString,
    gross_DesignedStorage :: DB.ByteString,
    live_TodayLiveStorage :: DB.ByteString,
    gross_TodayLiveStorage :: DB.ByteString,
    percent_Today_LiveStorage :: DB.ByteString,
    percent_SameDateLastYear_LiveStorage:: DB.ByteString
} deriving Show

data RegionEntry = RegionEntry {
    srNo :: Int,
    revenueRegion :: DB.ByteString,
    noDams :: Int,
    waterData :: RegionWaterData
} deriving Show

data RegionWaterData = RegionWaterData {
    dead_DesignedStorage :: Float,
    live_DesignedStorage :: Float,
    gross_DesignedStorage :: Float,
    live_TodayLiveStorage :: Float,
    gross_TodayLiveStorage :: Float,
    percent_LiveStorage_WRT_liveDesignedStorage :: DB.ByteString,
    percent_LiveStorage_WRT_sameDateLastYear :: DB.ByteString
} deriving Show