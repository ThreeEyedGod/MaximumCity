{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module WaterLevelLakes where

import WaterLevelHeaders
import Pdf.Document
import Data.Attoparsec.ByteString as Att
import Data.List
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

readPagesPdf :: PageNode -> Int -> Int -> IO T.Text 
readPagesPdf _ = []
readPagesPdf pn b e = 
      page <- pageNodePageByNum pn b 
      txt <- pageExtractText pageB

      pure $ mconcat [txtB , readPagesPdf pn (b+1) e]



-- | Right now only two consecutive pages
getPagesofPDFfromTo :: String -> Int -> Int -> IO T.Text 
getPagesofPDFfromTo url pageBeg pageEnd = do
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
      pageB <- pageNodePageByNum root pageBeg
      txtB <- pageExtractText pageB

      pageE <- pageNodePageByNum root pageEnd
      txtE <- pageExtractText pageE

      pure $ mconcat [txtB , txtE]

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
    x <- getPagesofPDFfromTo wlURL pageNum (pageNum + 1)
    pure $ TSE.encodeUtf8 x

getWaterLakeLevelBS :: Int -> IO ByteString
getWaterLakeLevelBS pageN = do 
    x <- getWaterLakeLevelPDFData pageN
    return $ x 

getWaterLakeLevelParsed :: ByteString -> Either String Page8Page9
getWaterLakeLevelParsed = parseOnly page8PageParser

-- | for now only pages 8-9 is being extracted 
getWLL :: IO (Either String Page8Page9)
getWLL = do
    x <- getWaterLakeLevelBS 7
    pure $ getWaterLakeLevelParsed x

-- Needs better error handling 
getAllDivData :: String -> IO [RegionEntry]
getAllDivData s = do
     w <- getWLL 
     case w of 
         Left err -> error "WLL getting failed "
         Right waterLevel -> do
                case s of 
                  "Major" -> 
                    case maybeHead $ (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel))                            
                        Nothing -> error "No Region data "
                  "Medium" -> 
                    case maybeHead $ (categoryProjectsLinedata (mediumMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure (categoryProjectsLinedata (mediumMaharashtraStateProjects waterLevel))                            
                        Nothing -> error "No Region data "
                  "Minor" -> 
                    case maybeHead $ (categoryProjectsLinedata (minorMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure (categoryProjectsLinedata (minorMaharashtraStateProjects waterLevel))                            
                        Nothing -> error "No Region data "
                  "All" -> 
                    case maybeHead $ (categoryProjectsLinedata (allDamsMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure (categoryProjectsLinedata (allDamsMaharashtraStateProjects waterLevel))                            
                        Nothing -> error "No Region data "
                  otherwise -> error "Ouch"

-- | revenueRegion == division
toTuple :: [RegionEntry] -> [(DB.ByteString, RegionWaterData)]
toTuple [] = []
toTuple (x:xs) = (revenueRegion x, waterData x):(toTuple xs)

extractDivisionData :: DB.ByteString -> IO [RegionEntry] -> IO (Maybe RegionWaterData)
extractDivisionData division xs = do 
    w <- xs 
    return $ lookup division (toTuple w)

getPercentLiveToday :: RegionWaterData -> PercentLiveStorage
getPercentLiveToday rwd = PercentLiveStorage { percent_Today = percent_LiveStorage_WRT_liveDesignedStorage rwd, 
            percent_LastYear = percent_LiveStorage_WRT_sameDateLastYear rwd
}

-- | for now hard coded to amravati 
getWaterLakeLevelForPlace_LiveToday_wrtStorage :: T.Text -> IO PercentLiveStorage
getWaterLakeLevelForPlace_LiveToday_wrtStorage pl = do
            -- nrstRegion <- findNearestRegionToPlace pl
            -- x <- extractDivisionData (TSE.encodeUtf8 nrstRegion) getAllDivDataMajor
            x <- extractDivisionData (TSE.encodeUtf8 "Kokan") $ getAllDivData "Major"
            case x of 
                Just y -> return $ getPercentLiveToday y
                Nothing -> error "Ouch "