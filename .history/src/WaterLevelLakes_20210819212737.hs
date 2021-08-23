{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where

import WaterLevelHeaders
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
 
-- | parser for the pdf
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

projectHeaderParser :: Parser T.Text
projectHeaderParser = do 
        rrh <- textGrabberDiscard
    return $ rrh


majorMaharashtraStateProjectsParser :: Parser CategoryProjects
majorMaharashtraStateProjects = do 

mediumMaharashtraStateProjectsParser :: Parser CategoryProjects
minorMaharashtraStateProjectsParser :: Parser CategoryProjects
allDamsMaharashtraStateProjectsParser :: Parser CategoryProjects

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
