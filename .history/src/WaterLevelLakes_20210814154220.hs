{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterLevelLakes where
import Pdf.Document

getLakeLevelat :: Float -> Float -> IO Text 
getLakeLevelat lat long 
 withPdfFile "input.pdf" $ \pdf ->
   doc <- document pdf
   catalog <- documentCatalog doc
   rootNode <- catalogPageNode catalog
   count <- pageNodeNKids rootNode
   print count
   page <- loadPageByNum rootNode 1
   text <- pageExtractText page
   print text