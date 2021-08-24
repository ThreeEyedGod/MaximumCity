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
import Data.Attoparsec.ByteString as Att
import Data.Attoparsec.ByteString.Char8 (skipSpace, isDigit_w8, manyTill, isHorizontalSpace, endOfLine, char, decimal, space, double, letter_ascii)

import JSONHelper
import ParserHelper

import qualified Data.Text as T
import Data.Text.Encoding as TSE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Data.ByteString.UTF8 as BSU  
import Control.Exception as X

import Network.HTTP.Conduit (simpleHttp)

import Data.Word
import Data.Time
import Helper 
--import qualified Data.Attoparsec.Text.Internal as API
--import Data.Attoparsec.ByteString.Char8.Parser

import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.
--import qualified Data.ByteString as B
import Data.Text.Encoding as TSE

wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"
getPdfAtThisURL ::  String -> IO B.ByteString
getPdfAtThisURL url = simpleHttp url `X.catch` exceptionHandler

getPageofPDFat :: String -> Int -> IO T.Text 
getPageofPDFat url pageNum = do
      pdfByteString <- getPdfAtThisURL url
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
      page <- pageNodePageByNum root pageNum
      txt <- pageExtractText page

      pure txt
 
getWaterLakeLevelPDFData :: Int -> IO ByteString
getWaterLakeLevelPDFData pageNum = do 
    x <- getPageofPDFat wlURL pageNum
    pure $ TSE.encodeUtf8 x

getWaterLakeLevelBS :: Int -> IO ByteString
getWaterLakeLevelBS pageN = do 
    x <- getWaterLakeLevelPDFData pageN
    return $ x 

getWaterLakeLevelParsed :: ByteString -> Either String Page8Page9
getWaterLakeLevelParsed = parseOnly page8PageParser

getWLL :: IO (Either String Page8Page9)
getWLL = do
    x <- getWaterLakeLevelBS 7
    pure $ getWaterLakeLevelParsed x

getAllDivDataMajor :: IO [RegionEntry]
getAllDivDataMajor = do
     w <- getWLL 
     case w of 
         Left err -> error "WLL getting failed "
         Right waterLevel -> do
                    case maybeHead $ (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel))                            
                        Nothing -> error "No Region data "

getMajorPercentStatusForDivision :: DB.ByteString -> IO PercentLiveStorage
getMajorPercentStatusForDivision div = do 
    x <- getAllDivDataMajor
    pattern RegionEntry a b c <- 

extractPercentData :: [RegionEntry] -> PercentLiveStorage
extractPercentData [_,RegionEntry  ]

-- This pattern will fail if the list it matches against
-- isn't exactly of the form it's expecting.
pattern VerySpecificList a b c <- [(0, a), (b, "hello"), (1, [_]), c]

data RegionEntry = RegionEntry {
    srNo :: Int,
    revenueRegion :: DB.ByteString,
    waterData :: RegionWaterData
} deriving Show

-- Here is how the above pattern synonym might be used:
extractMyData :: [(Int, String)] -> Maybe (String, Int, Int, String)
extractMyData (VerySpecificList str0 n0 (n1, str1)) = Just (str0, n0, n1, str1)
extractMyData _ = Nothing

 