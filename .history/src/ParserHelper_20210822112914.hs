{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ParserHelper where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as DB
import Data.ByteString.UTF8 as BSU  

import Data.Word
import Data.Time
import qualified Data.Attoparsec.ByteString.Char8 as A

import Control.Applicative
import Data.Text.Encoding as TSE

-- | parser for the pdf
textTillCRNL :: Parser String
textTillCRNL = do 
    A.endOfLine
    tgD <- A.takeWhile (\c -> c /= '\n' && c /= '\r')
    return $ toString tgD

{- statusDateParser :: A.Parser String
statusDateParser = do 
    A.endOfLine
    x <- textTillCRNL
    return $ TSE.decodeUtf8 x 
 -}
{- topicTitleParser :: A.Parser DB.ByteString
topicTitleParser = textGrabberDiscard
   
revenueRegionHeaderParser :: A.Parser DB.ByteString
revenueRegionHeaderParser = textGrabberDiscard

colHeaderParser :: A.Parser [ColHeader]
colHeaderParser = A.manyTill textGrabberDiscard A.endOfLine

colNumbersParser :: A.Parser [ColumnNos]
colNumbersParser = A.manyTill parseColHeader A.endOfLine

parseColHeader :: A.Parser ColumnNos
parseColHeader = do
    d1 <- A.decimal
    return $ d1 

projectHeaderParser :: A.Parser DB.ByteString
projectHeaderParser = textGrabberDiscard -}
{- 
majorMaharashtraStateProjectsParser :: A.Parser CategoryProjects
majorMaharashtraStateProjectsParser = do 
    mjrCH <- projectHeaderParser
    A.endOfLine
    return $ CategoryProjects mjrCH

parsePage8Page9 :: A.Parser Page8Page9
parsePage8Page9 = do
    sd <- statusDateParser 
    A.endOfLine
    tt <- topicTitleParser
    A.endOfLine
    rrh <- revenueRegionHeaderParser
    A.endOfLine
    ch <- colHeaderParser
    A.endOfLine
    cn <- colNumbersParser
    A.endOfLine
    maMSP <- majorMaharashtraStateProjectsParser
    A.endOfLine
    return $ Page8Page9 sd tt rrh ch cn maMSP
 -}