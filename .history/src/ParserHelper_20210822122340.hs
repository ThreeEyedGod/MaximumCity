{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ParserHelper where

import qualified Data.ByteString.Lazy as DL
import qualified Data.ByteString
import Data.ByteString.UTF8 as DBU 

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.Attoparsec.Text (takeWhile)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Word (Word32)
import Prelude hiding (takeWhile)
import WaterLevelHeaders

-- | parser for the pdf
textTillCRNL :: Parser Text
textTillCRNL = do 
    endOfLine
    tgD <- takeWhile (\c -> c /= '\n' && c /= '\r')
    return (pack tgD)

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