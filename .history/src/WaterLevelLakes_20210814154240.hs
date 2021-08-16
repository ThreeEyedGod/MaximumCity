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

getLakeLevelat :: Float -> Float -> IO T.Text 
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