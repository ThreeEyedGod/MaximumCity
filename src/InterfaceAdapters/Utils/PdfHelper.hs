{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-@ LIQUID "--skip-module" @-}

module InterfaceAdapters.Utils.PdfHelper where

import Pdf.Document
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Control.Exception as X
import Network.HTTP.Conduit (simpleHttp)
import InterfaceAdapters.Utils.JSONHelper

type FirstPage = Int
type LastPage = Int
type URL = String

getPdfAtThisURL ::  String -> IO B.ByteString
getPdfAtThisURL url = simpleHttp url `X.catch` exceptionHandler


readPagesPdf :: PageNode -> FirstPage -> LastPage -> IO T.Text 
readPagesPdf pn a b  | a > b = pure ""
                     | otherwise = do 
                            page <- pageNodePageByNum pn a
                            txt <- pageExtractText page
                            nxt <- readPagesPdf pn (a+1) b
                            pure $ mconcat [txt, nxt]

getPDFProperties :: URL -> IO (PageNode, Int, Maybe T.Text)
getPDFProperties url  = do
      pdfByteString <- getPdfAtThisURL url
      pdf <- fromBytes $ B.toStrict pdfByteString
      doc <- document pdf

      maybe_info <- documentInfo doc
      title <- case maybe_info of
          Nothing -> pure Nothing
          Just info -> infoTitle info

      root <- documentCatalog doc >>= catalogPageNode
      total <- pageNodeNKids root

      return (root, total, title)

getPagesofPDFfromTo :: URL -> FirstPage -> LastPage -> IO T.Text 
getPagesofPDFfromTo url pageBeg pageEnd = do
      (root, total, title) <- getPDFProperties url
      readPagesPdf root pageBeg pageEnd
