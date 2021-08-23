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
--import qualified Data.ByteString as B



type Lakename = T.Text

 
data CategoryProjects = CategoryProjects {
    categoryHeader :: ProjectHeader,
    categoryProjects :: [RegionEntry],
    totalCategoryProjects :: TotalProjectFooter
} deriving Show

textGrabberDiscard :: Parser T.Text
textGrabberDiscard = do 
    tgD <- takeWhile1 (not . endOfLine)
    return tgD

statusDateParser :: Parser T.Text
statusDateParser = do 
    sd <- textGrabberDiscard
    return $ sd

topicTitleParser :: Parser T.Text
topicTitleParser = do 
    tt <- textGrabberDiscard
    return $ tt

revenueRegionHeaderParser :: Parser T.Text
revenueRegionHeaderParser = do
    rrh <- textGrabberDiscard
    return $ rrh

colHeaderParser :: Parser [ColHeader]
colHeaderParser = manyTill textGrabberDiscard endOfLine

colNumbersParser :: Parser [ColumnNos]
colNumbersParser = manyTill parseColHeader endOfLine

parseColHeader :: Parser ColumnNos
parseColHeader = 
    d1 <- decimal
    return $ ColumnNos d1 

parsePage8Page9 :: Parser Page8Page9
parsePage8Page9 = do
    sd <- statusDateParser 
    char '\n'
    tt <- topicTitleParser
    char '\n'
    rrh <- revenueRegionHeaderParser
    char '\n'
    ch <- colHeaderParser
    char '\n'
    cn <- colNumbersParser
    char '\n'
    maMSP <- majorMaharashtraStateProjectsParser
    char '\n'
    meMSP <- mediumMaharashtraStateProjectsParser
    char '\n'
    miMSP <- minorMaharashtraStateProjectsParser
    char '\n'
    aMSP <- allDamsMaharashtraStateProjectsParser
    return $ Page8Page9 sd tt rrh ch cn maMSP meMSP miMSP aMSP

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

type ColumnNos = Int

data ProjectHeader = ProjectHeader {
    projectsHeader :: T.Text
} deriving Show

data TotalProjectFooter = TotalProjectFooter {
    projectsFooter :: T.Text,
    waterDataTotal :: RegionWaterData
} deriving Show

type ColHeader = T.Text

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