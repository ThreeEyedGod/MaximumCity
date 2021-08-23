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

import Data.Word
import Data.Time
import qualified Data.Attoparsec.ByteString.Char8 as A
--import Data.Attoparsec.ByteString.Char8.Parser

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
textGrabberDiscard :: A.Parser B.ByteString
textGrabberDiscard = do 
    tgD <- A.takeWhile (\c -> c /= '\n' && c /= '\r')
    return tgD

checkParser :: A.Parser B.ByteString
checkParser = textGrabberDiscard

statusDateParser :: A.Parser B.ByteString
statusDateParser = textGrabberDiscard

topicTitleParser :: A.Parser B.ByteString
topicTitleParser = textGrabberDiscard
   
revenueRegionHeaderParser :: A.Parser B.ByteString
revenueRegionHeaderParser = textGrabberDiscard

colHeaderParser :: A.Parser [ColHeader]
colHeaderParser = A.manyTill textGrabberDiscard A.endOfLine

colNumbersParser :: A.Parser [ColumnNos]
colNumbersParser = A.manyTill parseColHeader A.endOfLine

parseColHeader :: A.Parser ColumnNos
parseColHeader = do
    d1 <- A.decimal
    return $ d1 

projectHeaderParser :: A.Parser B.ByteString
projectHeaderParser = textGrabberDiscard

majorMaharashtraStateProjectsParser :: A.Parser CategoryProjects
majorMaharashtraStateProjectsParser = do 
    mjrCH <- projectHeaderParser
    A.char '\n'
    return $ CategoryProjects mjrCH

parsePage8Page9 :: A.Parser Page8Page9
parsePage8Page9 = do
    sd <- statusDateParser 
    A.char '\n'
    tt <- topicTitleParser
    A.char '\n'
    rrh <- revenueRegionHeaderParser
    A.char '\n'
    ch <- colHeaderParser
    A.char '\n'
    cn <- colNumbersParser
    A.char '\n'
    maMSP <- majorMaharashtraStateProjectsParser
    A.char '\n'
    return $ Page8Page9 sd tt rrh ch cn maMSP
