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

test1 :: DB.ByteString
test1 = "\nStatus as on Date: 31 Aug 2021 \nMajor, Medium and Minor Projects Live Storage Comparison\nRevenue Region\nSR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE (Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. DESIGNED W.R.T. DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10\nMajor Projects\n1 Amravati 10 589.95 2363.93 2953.88 1810.86 2423.47 76.6% 89.68%\n2 Aurangabad 45 1444.86 4505.36 5950.21 2437.85 3870.65 54.11% 80.39%\n3 Kokan 11 123.37 2459.4 2582.77 2238.32 2354.05 91.01% 84.31%\n4 Nagpur 16 839.5 3462.92 4302.42 1971.82 2811.32 56.94% 85.33%\n5 Nashik 24 529.18 3738.75 4267.93 2487.1 3016.26 66.52% 83.7%\n6 Pune 35 2950.74 
12444.02 15394.75 10283.27 13234.66 82.64% 94.85%\nTotal Major Projects 141 6477.59 28974.36 35451.96 21229.22 27710.41 73.27% 88.71%\nMedium Projects\n1 Amravati 25 107.09 677.02 784.11 401.13 483.94 59.25% 76.85%\n2 Aurangabad 81 169.21 1056.42 1225.63 312.36 416.07 29.57% 53.95%\n3 Kokan 7 9.97 487.19 497.16 371.3 382.42 76.21% 90.81%\n4 Nagpur 42 73.53 635.16 708.7 202.19 231.95 31.83% 63.99%\n5 Nashik 53 218.43 1192.72 1411.15 401.7 568.12 33.68% 69.35%\n6 Pune 50 97.21 1362.71 1459.92 662.68 717.03 48.63% 69.86%\nTotal Medium Projects 258 675.44 5411.22 6086.66 2351.35 2799.53 43.45% 68.71%\nMinor Projects\n1 Amravati 411 104.01 1033.59 1137.6 248.08 297.44 24.0% 30.58%\n2 Aurangabad 838 240.97 1810.49 2051.46 326.82 430.94 18.05% 30.08%\n3 Kokan 158 28.1 563.47 591.57 261.77 280.86 46.46% 62.06%\n4 Nagpur 326 59.33 508.93 568.27 101.7 132.3 19.98% 63.88%\n5 Nashik 494 132.05 1072.47 1204.52 129.81 180.13 12.1% 47.08%\n6 Pune 641 89.95 1404.68 1494.63 121.27 143.36 8.63% 22.37%\nTotal Minor Projects 2868 654.42 6393.63 7048.05 1189.46 1465.03 18.6% 36.83%\nMaharashtra State (All dams) Projects\n1 Amravati 446 801.06 4074.53 4875.59 2460.07 3204.85 60.38% 72.55%\n2 Aurangabad 964 1855.04 7372.26 9227.3 3077.03 4717.67 41.74% 64.24%\n3 Kokan 176 161.44 3510.06 3671.5 2871.4 3017.34 81.8% 81.64%\n4 Nagpur 384 972.37 4607.01 5579.38 2275.71 3175.56 49.4% 80.01%\nREVENUE REVENUE REGION REGION\nSR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE\n(Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. W.R.T. DESIGNED DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10\n5 Nashik 571 879.65 6003.95 6883.6 3018.61 3764.51 50.28% 74.31%\n6 Pune 726 3137.9 15211.4 18349.3 11067.22 14095.05 72.76% 85.92%\nTotal Maharashtra State (All dams) Projects 3267 7807.45 40779.22 48586.68 24770.03 31974.97 60.74% 77.92%\nREVENUE REGION REVENUE REGION"

data Page8Page9 = Page8Page9 {
    pageHeaders :: SectionHeaders,
    majorMaharashtraStateProjects :: CategoryProjects,
    mediumMaharashtraStateProjects :: CategoryProjects,
    minorMaharashtraStateProjects :: CategoryProjects,
    allDamsMaharashtraStateProjects :: CategoryProjects
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
