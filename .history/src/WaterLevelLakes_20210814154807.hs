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


wlURL :: String
jsonWLURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"
getDocatThisURL ::  IO B.ByteString
getDocatThisURL = simpleHttp jsonWLURL `X.catch` statusExceptionHandler

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