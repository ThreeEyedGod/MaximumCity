{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterfaceAdapters.Water.MH.Core.WaterLevelLakes where

import InterfaceAdapters.Water.MH.Core.WaterLevelHeaders
import InterfaceAdapters.Utils.PdfHelper
import InterfaceAdapters.Utils.Helper
import Data.Attoparsec.ByteString as Att
import qualified Data.List as DL
import Data.Attoparsec.ByteString.Char8 (skipSpace, isDigit_w8, manyTill, isHorizontalSpace, endOfLine, char, decimal, space, double, letter_ascii)
import InterfaceAdapters.IP.GeoLatLong (getLatLongforThis)
import Data.ByteString.Search as DBS

import Naqsha.Geometry
import Naqsha.Geometry.Spherical
import InterfaceAdapters.Utils.JSONHelper
import InterfaceAdapters.Water.MH.Core.ParserHelper

import qualified Data.Text as T
import Data.Char
import Data.Text.Encoding as TSE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Data.ByteString.UTF8 as BSU
import Control.Exception as X

import Data.Word
import Data.Time
import InterfaceAdapters.Utils.Helper

import Control.Applicative
import Data.Text.Encoding as TSE

-- | Begin Maharashtra  ********
wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"


getMHWaterURL :: IO (Either String String)
getMHWaterURL = do
  url <- getKey "MH_WATER_DATA"
  case url of
    Left msg -> pure $ Left "Error : In URL in environment for MH water data"
    Right pagelink -> return $ Right pagelink

getMHWaterPageLinkPage :: IO (Either String String)
getMHWaterPageLinkPage = do
  url <- getKey "MH_WATER_DATA_PAGE"
  case url of
    Left msg -> pure $ Left "Error : In environment for MH water data url page number"
    Right pagenum -> return $ Right pagenum

getWaterLakeLevelPDFData :: Int -> IO ByteString
getWaterLakeLevelPDFData pageNum = do
    y <- getKey "MH_WATER_DATA"
    x <- getKey "MH_WATER_DATA_PAGE"
    let pagelink = read $ getKeyEither y :: String 
    let pagenum2 = read $ getKeyEither x :: Int 
    -- x <- getPagesofPDFfromTo wlURL pageNum (pageNum + 1)
    x <- getPagesofPDFfromTo pagelink pagenum2 (pagenum2 + 1)
    pure $ TSE.encodeUtf8 x

getWaterLakeLevelBS :: Int -> IO ByteString
getWaterLakeLevelBS pageN = do
    x <- getWaterLakeLevelPDFData pageN
    let repwithBS = "" :: ByteString
    let toBeExcisedBS = "\nREVENUE REVENUE REGION REGION\nSR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE (Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. W.R.T. DESIGNED DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10" :: ByteString
    let x1 = DBS.replace toBeExcisedBS repwithBS x
    pure $ DB.concat . B.toChunks $ x1

getWaterLakeLevelParsed :: ByteString -> Either String Page8Page9
getWaterLakeLevelParsed = parseOnly page8PageParser

-- | for now only pages 8-9 is being extracted 
-- | this changed to page 12-13 - need to make the software intelligent enough to track those changes ! 
-- | externalize this to AWS environment ?
-- | moved to 10 ! Back to 11
getWLL :: IO (Either String Page8Page9)
getWLL = do
    x <- getWaterLakeLevelBS 10
    pure $ getWaterLakeLevelParsed x

getSpecificProjectSizeDataCategoryProjects :: String -> Page8Page9 -> Maybe CategoryProjects
getSpecificProjectSizeDataCategoryProjects s p8p9
    | s == "Major" = Just (majorMaharashtraStateProjects p8p9)
    | s == "Medium" = Just (mediumMaharashtraStateProjects p8p9)
    | s == "Minor" = Just (minorMaharashtraStateProjects p8p9)
    | s == "All" = Just (allDamsMaharashtraStateProjects p8p9)
    | otherwise = Nothing

getSpecificProjectSizeDataCategoryProjectsLineData :: Maybe CategoryProjects -> Maybe [RegionEntry]
getSpecificProjectSizeDataCategoryProjectsLineData Nothing = Nothing
getSpecificProjectSizeDataCategoryProjectsLineData (Just cpjs) = Just (categoryProjectsLinedata cpjs)

getAllDivData :: String -> IO (Maybe [RegionEntry])
getAllDivData s = do
     w <- getWLL
     case w of
         Left err -> return Nothing
         Right waterLevel -> do
                let cpjs = getSpecificProjectSizeDataCategoryProjects s waterLevel
                let ld = getSpecificProjectSizeDataCategoryProjectsLineData cpjs
                case ld of
                    Nothing -> return Nothing
                    Just linedata -> pure (Just linedata)


-- | revenueRegion == division
toTuple :: [RegionEntry] -> [(DB.ByteString, RegionWaterData)]
toTuple = map (\ x -> (revenueRegion x, waterData x))

extractDivisionData :: DB.ByteString -> Maybe [RegionEntry] -> IO (Maybe RegionWaterData)
extractDivisionData _ Nothing = return Nothing
extractDivisionData division (Just xs) = do
    let w = xs
    return $ lookup division (toTuple w)

getPercentLiveToday :: RegionWaterData -> PercentLiveStorage
getPercentLiveToday rwd = PercentLiveStorage { percent_Today = percent_LiveStorage_WRT_liveDesignedStorage rwd,
            percent_LastYear = percent_LiveStorage_WRT_sameDateLastYear rwd
}

isComma :: Char -> Bool
isComma c = c == ','

items_sepByComma                   :: String -> [String]
{-# NOINLINE [1] items_sepByComma #-}
items_sepByComma s                 =  case dropWhile {-partain:Char.-}isComma s of
                                "" -> []
                                s' -> w : items_sepByComma s''
                                      where (w, s'') =
                                             Prelude.break {-partain:Char.-}isComma s'

extLatLon :: T.Text -> (Angle, Angle)
extLatLon str =  (x , y) where
    z = items_sepByComma $ T.unpack str
    (x,y) = (degree $ toRational (read (head z) :: Float), degree $ toRational (read (last z) :: Float))

mkGeo :: (Angle, Angle) -> Geo
mkGeo (l1, l2) = Geo (lat l1) (lon l2)

regionLatLong :: [(String, Geo)]
regionLatLong = [("Aurangabad", mkGeo (degree 19.88461, degree 75.33816)), ("Amravati", mkGeo (degree 20.95085, degree 77.75075)), ("Kokan", mkGeo (degree 17.22625, degree 73.32031)), ("Nagpur", mkGeo (degree 21.15655, degree 79.08074)),("Nashik", mkGeo (degree 19.99482, degree 73.79970)), ("Pune", mkGeo (degree 18.51610, degree 73.86295))]

-- | test data "19.0748,72.8856"
getDistanceToAllRegions :: T.Text -> [(String, Double)]
getDistanceToAllRegions plLatLon = map (\(str, to) -> (str , distance (mkGeo $ extLatLon plLatLon) to)) regionLatLong

findNearestRegionToPlace :: String -> IO String
findNearestRegionToPlace pl = do
    c <- getLatLongforThis pl
    let di = getDistanceToAllRegions c
    let mindi = minimum $ map snd di
    let rg = fst $ head $ filter ((== mindi) . snd) di
    pure rg

getWaterLakeLevelForPlace_LiveToday_wrtStorage :: T.Text -> IO (Maybe Region, Maybe PercentLiveStorage)
getWaterLakeLevelForPlace_LiveToday_wrtStorage pl = do
    nrstRegion <- findNearestRegionToPlace (T.unpack pl)
    maybe_allDivData <- getAllDivData "All"
    x <- extractDivisionData (BSU.fromString nrstRegion) maybe_allDivData
    case x of
        Just y -> return  ( Just (BSU.fromString nrstRegion) , Just $ getPercentLiveToday y)
        Nothing -> return ( Just (BSU.fromString nrstRegion) , Nothing)

-- | End Maharashtra  ********
