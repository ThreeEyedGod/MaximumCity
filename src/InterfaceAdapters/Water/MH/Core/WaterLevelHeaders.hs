{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterfaceAdapters.Water.MH.Core.WaterLevelHeaders where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Prelude

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative

type Lakename = DB.ByteString
type Region = DB.ByteString
type ColHeader = DB.ByteString
type ColumnNos = Int
type ProjectHeader = DB.ByteString

data Page8Page9 = Page8Page9 {
    pageHeaders :: SectionHeaders
   ,majorMaharashtraStateProjects :: CategoryProjects
   ,mediumMaharashtraStateProjects :: CategoryProjects
   ,minorMaharashtraStateProjects :: CategoryProjects
   ,allDamsMaharashtraStateProjects :: CategoryProjects
} deriving Show

data JustSectionHeaders = JustSectionHeaders {
    statusDate :: DB.ByteString,
    topicTitle :: DB.ByteString,
    revenueRegionHeader :: DB.ByteString
} deriving Show

data SectionHeaders = SectionHeaders {
    justSectionHeaders :: JustSectionHeaders,
    colHeader :: ColHeader,
    colNumbers :: [ColumnNos]
} deriving Show

data CategoryProjects = CategoryProjects {
    categoryHeader :: ProjectHeader,
    categoryProjectsLinedata :: [RegionEntry],
    totalCategoryProjects :: TotalProjectFooter
} deriving Show

data TotalProjectFooter = TotalProjectFooter {
    projectsFooter :: DB.ByteString,
    waterDataTotal :: RegionWaterData
} deriving Show

data RevenueRegionHeader = RevenueRegionHeader {
    srNo :: DB.ByteString,
    revenueRegionHeader :: DB.ByteString,
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
    waterData :: RegionWaterData
} deriving Show

data RegionWaterData = RegionWaterData {
    noDams :: Int,
    dead_DesignedStorage :: Double,
    live_DesignedStorage :: Double,
    gross_DesignedStorage :: Double,
    live_TodayLiveStorage :: Double,
    gross_TodayLiveStorage :: Double,
    percent_LiveStorage_WRT_liveDesignedStorage :: DB.ByteString,
    percent_LiveStorage_WRT_sameDateLastYear :: DB.ByteString
} deriving Show

data PercentLiveStorage = PercentLiveStorage {
    percent_Today :: DB.ByteString,
    percent_LastYear :: DB.ByteString
} deriving Show