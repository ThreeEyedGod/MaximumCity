{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module PdfHelper where

import Pdf.Document
import qualified Data.List as DL

import JSONHelper

import qualified Data.Text as T
import Data.Char
import Data.Text.Encoding as TSE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Helper 

import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.
--import qualified Data.ByteString as B

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