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
import PdfHelper
import Data.Attoparsec.ByteString as Att
import qualified Data.List as DL
import Data.Attoparsec.ByteString.Char8 (skipSpace, isDigit_w8, manyTill, isHorizontalSpace, endOfLine, char, decimal, space, double, letter_ascii)

import Naqsha.Geometry
import Naqsha.Geometry.Spherical
import JSONHelper
import ParserHelper

import qualified Data.Text as T
import Data.Char
import Data.Text.Encoding as TSE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Data.ByteString.UTF8 as BSU  
import Control.Exception as X

import Data.Word
import Data.Time
import Helper 

import Control.Applicative
import Data.Text.Encoding as TSE
import GeoIpAPI


-- | Begin Maharashtra  ********
wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"

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
getAllDivData :: String -> IO Maybe [RegionEntry]
getAllDivData s = do
     w <- getWLL 
     case w of 
         -- Left err -> error "WLL getting failed "
         Left err -> Nothing
         Right waterLevel -> do
                case s of 
                  "Major" -> 
                    case maybeHead $ (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure Just (categoryProjectsLinedata (majorMaharashtraStateProjects waterLevel))                            
                        Nothing -> Nothing -- error "No Region data "
                  "Medium" -> 
                    case maybeHead $ (categoryProjectsLinedata (mediumMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure Just (categoryProjectsLinedata (mediumMaharashtraStateProjects waterLevel))                            
                        Nothing -> Nothing -- error "No Region data "
                  "Minor" -> 
                    case maybeHead $ (categoryProjectsLinedata (minorMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure Just (categoryProjectsLinedata (minorMaharashtraStateProjects waterLevel))                            
                        Nothing -> Nothing -- error "No Region data "
                  "All" -> 
                    case maybeHead $ (categoryProjectsLinedata (allDamsMaharashtraStateProjects waterLevel)) of 
                        Just isThere -> pure Just (categoryProjectsLinedata (allDamsMaharashtraStateProjects waterLevel))                            
                        Nothing -> Nothing -- error "No Region data "
                  otherwise -> Nothing --error "Ouch"

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

isComma :: Char -> Bool
isComma c = (c == ',')

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
    (x,y) = (degree $ toRational $ (read (head z) :: Float), degree $ toRational $ (read (last z) :: Float))

mkGeo :: (Angle, Angle) -> Geo
mkGeo (l1, l2) = Geo (lat $ l1) (lon $ l2)

regionLatLong :: [(String, Geo)]
regionLatLong = [("Aurangabad", mkGeo (degree 19.88461, degree 75.33816)), ("Amravati", mkGeo (degree 20.95085, degree 77.75075)), ("Kokan", mkGeo (degree 17.22625, degree 73.32031)), ("Nagpur", mkGeo (degree 21.15655, degree 79.08074)),("Nashik", mkGeo (degree 19.99482, degree 73.79970)), ("Pune", mkGeo (degree 18.51610, degree 73.86295))]

-- | test data "19.0748,72.8856"
getDistanceToAllRegions :: T.Text -> [(String, Double)]
getDistanceToAllRegions plLatLon = map (\(str, to) -> (str , distance (mkGeo $ extLatLon plLatLon) to)) regionLatLong

findNearestRegionToPlace :: String -> IO String
findNearestRegionToPlace pl = do
    c <- getLatLongforThis pl 
    let di = getDistanceToAllRegions c
    let mindi = minimum $ map (\x -> snd x) di
    let rg = fst $ head $ filter ((== mindi) . snd) di
    pure $ rg

getWaterLakeLevelForPlace_LiveToday_wrtStorage :: T.Text -> IO (Region, PercentLiveStorage)
getWaterLakeLevelForPlace_LiveToday_wrtStorage pl = do
            nrstRegion <- findNearestRegionToPlace (T.unpack pl)
            case getAllDivData "All" of
                Nothing -> Nothing
                Just arrRE ->  do 
                    --x <- extractDivisionData (BSU.fromString nrstRegion) $ getAllDivData "All"
                    x <- extractDivisionData (BSU.fromString nrstRegion) $ getAllDivData "All"
                    case x of 
                        Just y -> return $ ((BSU.fromString nrstRegion) , getPercentLiveToday y)
                        Nothing -> Nothing 

-- | End Maharashtra  ********
