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
import qualified Data.Attoparsec.ByteString.Char8 as A
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
    tgD <- A.takeWhile (\c -> c /= '\n' && c /= '\r')
    return tgD

checkParser :: Parser T.Text
checkParser = textGrabberDiscard

statusDateParser :: Parser T.Text
statusDateParser = textGrabberDiscard

topicTitleParser :: Parser T.Text
topicTitleParser = textGrabberDiscard
   
revenueRegionHeaderParser :: Parser T.Text
revenueRegionHeaderParser = textGrabberDiscard

colHeaderParser :: Parser [ColHeader]
colHeaderParser = A.manyTill textGrabberDiscard endOfLine

colNumbersParser :: Parser [ColumnNos]
colNumbersParser = manyTill parseColHeader endOfLine

parseColHeader :: Parser ColumnNos
parseColHeader = do
    d1 <- decimal
    return $ d1 

projectHeaderParser :: Parser T.Text
projectHeaderParser = textGrabberDiscard

majorMaharashtraStateProjectsParser :: Parser CategoryProjects
majorMaharashtraStateProjectsParser = do 
    mjrCH <- projectHeaderParser
    char '\n'
    return $ CategoryProjects mjrCH

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
