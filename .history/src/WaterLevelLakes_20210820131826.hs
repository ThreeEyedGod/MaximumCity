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

checkParser :: Parser T.Text
checkParser = textGrabberDiscard

statusDateParser :: Parser T.Text
statusDateParser = textGrabberDiscard
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
parseColHeader = do
    d1 <- decimal
    return $ d1 

projectHeaderParser :: Parser T.Text
projectHeaderParser = do 
    phP <- textGrabberDiscard
    return $ phP

majorMaharashtraStateProjectsParser :: Parser CategoryProjects
majorMaharashtraStateProjectsParser = do 
    mjrCH <- projectHeaderParser
    char '\n'
    --mjrRE <- regionEntryParser
    --char '\n'
    --mjrTotal <- totalProjectsParser
    return $ CategoryProjects mjrCH

{- mediumMaharashtraStateProjectsParser :: Parser CategoryProjects
minorMaharashtraStateProjectsParser :: Parser CategoryProjects
allDamsMaharashtraStateProjectsParser :: Parser CategoryProjects
 -}
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
    return $ Page8Page9 sd tt rrh ch cn maMSP
