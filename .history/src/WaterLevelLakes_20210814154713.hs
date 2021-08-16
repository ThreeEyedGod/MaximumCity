{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where
import Pdf.Document
import qualified Data.Text as T


jsonWLURL :: String
jsonWLURL = "https://freegeoip.app/json/"
getGeoIpforThis ::  IO B.ByteString
getGeoIpforThis = simpleHttp jsonGeoIpURL `X.catch` statusExceptionHandler
jsonPositionStackURL :: String
jsonPositionStackURL = "http://api.positionstack.com/v1/forward?access_key="




getLakeLevelatLatLong :: Float -> Float -> IO T.Text 
getLakeLevelat lat long = do
    withPdfFile "input.pdf" $ \pdf ->
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    print count
    page <- loadPageByNum rootNode 1
    text <- pageExtractText page
    print text