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

import Naqsha
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
readPagesPdf pn a b  | a > b = pure ""
                     | otherwise = do 
                            page <- pageNodePageByNum pn a
                            txt <- pageExtractText page
                            nxt <- readPagesPdf pn (a+1) b
                            pure $ mconcat [txt, nxt]

getPDFProperties :: String -> IO (PageNode, Int, Maybe T.Text)
getPDFProperties url  = do
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

getPagesofPDFfromTo :: String -> Int -> Int -> IO T.Text 
getPagesofPDFfromTo url pageBeg pageEnd = do
      (root, total, title) <- getPDFProperties url
      extract <- readPagesPdf root pageBeg pageEnd
      pure extract

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

extLatLon :: T.Text -> (Angle, Angle)
extLatLon str = pure $ (degree 19, degree 20)
mkGeo :: (Angle, Angle) -> Geo
mkGeo (l1, l2) = Geo (lat $ l1) 

type PlaceName = T.Text 
type Region = DB.ByteString
regionLatLong :: [(String, Geo)]
regionLatLong = [("Thane", Geo (lat $ degree 19) (lon $ degree 73)), ("Amravati", Geo (lat $ degree 23) (lon $ degree 75))]
findNearestRegionToPlace :: PlaceName -> Region
findNearestRegionToPlace pl = do
    c <- getLatLongforThis pl 
    (plLat, plLon) <- extLatLon c
    rg <- fst . map (\to -> distance c to) regionLatLong
    pure $ rg

-- | for now hard coded to amravati 
getWaterLakeLevelForPlace_LiveToday_wrtStorage :: T.Text -> IO PercentLiveStorage
getWaterLakeLevelForPlace_LiveToday_wrtStorage pl = do
            -- nrstRegion <- findNearestRegionToPlace pl
            -- x <- extractDivisionData (TSE.encodeUtf8 nrstRegion) $ getAllDivData "Medium"
            x <- extractDivisionData (TSE.encodeUtf8 "Kokan") $ getAllDivData "Major"
            case x of 
                Just y -> return $ getPercentLiveToday y
                Nothing -> error "Ouch "
