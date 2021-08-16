{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where

--import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Document
import qualified Data.Text as T
import Prelude

wlURL :: String
wlURL = "https://d3suziiw6thyiv.cloudfront.net/reports/storage-comparison/standard/pdf/view?MenuID=1317"
getDocatThisURL ::  IO B.ByteString
getDocatThisURL = simpleHttp wlURL `X.catch` statusExceptionHandler

Type Lakename = Lakename 

getLakeLevelatLatLong :: Lakename -> IO T.Text 
getLakeLevelat ln = do
  withPdfFile wlURL $ \pdf ->
    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        fail "need password"
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    print count
    -- the first page of the document
    page <- pageNodePageByNum rootNode 0
    -- extract text
    txt <- pageExtractText page
    pure txt
  