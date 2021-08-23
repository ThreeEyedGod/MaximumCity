
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserHelper where
import Debug.Trace 

import Data.Attoparsec.ByteString as Att
import Data.Attoparsec.ByteString.Char8 (endOfLine, char, decimal, space, double)
import qualified Data.Word8 as W
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString ( ByteString )
import Control.Monad
import Data.Char
import Prelude hiding (takeWhile)
import WaterLevelHeaders

pattern LF :: Word8
pattern LF = 10

pattern CR :: Word8
pattern CR = 13

pattern PCT :: Word8
pattern PCT = 37
-- | parser for the pdf
lFtextTillCRNL :: Parser ByteString
lFtextTillCRNL = endOfLine >> takeWhile (\c -> c /= LF && c /= CR)

textTillCRNL :: Parser ByteString
textTillCRNL = takeWhile (\c -> c /= LF && c /= CR)

textTillPCT :: Parser ByteString
textTillPCT = takeWhile (\c -> c /= PCT)

colHeaderParser :: Parser ColHeader
colHeaderParser  = string "\nSR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE (Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. DESIGNED W.R.T. DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n"

colNumbersParser :: Parser [ColumnNos]
colNumbersParser = do 
    x <- decimal `sepBy1` (char ' ')
    endOfLine
    return x

justSectionHeadersParser :: Parser JustSectionHeaders
justSectionHeadersParser  = do
    a <- lFtextTillCRNL
    b <- lFtextTillCRNL
    c <- lFtextTillCRNL
    return $ JustSectionHeaders a b c

sectionHeadersParser :: Parser SectionHeaders
sectionHeadersParser = do
    a <- justSectionHeadersParser
    b <- colHeaderParser
    c <- colNumbersParser
    return $ SectionHeaders a b c

categoryHeaderParser :: Parser ProjectHeader
categoryHeaderParser = lFtextTillCRNL

-- | 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%
regionWaterDataParser :: Parser RegionWaterData
regionWaterDataParser = do
    a <- double 
    _ <- space
    b <- double
    _ <- space
    c <- double
    _ <- space
    d <- double
    _ <- space
    e <- double
    _ <- space
    f <- textTillPCT
    _ <- (letter_ascii 37) >> space
    g <- textTillPCT
    endOfLine
    return $ RegionWaterData a b c d e f g 

-- | \n1 Amravati 10 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%\n
regionEntryParser :: Parser RegionEntry
regionEntryParser = do
    endOfLine
    a <- decimal
    _ <- space 
    b <- lFtextTillCRNL
    _ <- space
    c <- decimal
    _ <- space
    d <- regionWaterDataParser
    endOfLine
    return $ RegionEntry a b c d

-- | Test 
parseCHP :: ByteString -> IO ()
parseCHP = parseTest categoryHeaderParser

parseTest0 :: ByteString -> Either String ByteString
parseTest0 = parseOnly lFtextTillCRNL 

parseTest1 :: ByteString -> Either String ByteString
parseTest1 = parseOnly textTillCRNL 

parseTest2 :: ByteString -> IO ()
parseTest2 = parseTest justSectionHeadersParser

parseTest3 :: ByteString -> Either String ByteString
parseTest3 = parseOnly colHeaderParser

parseTest4 :: ByteString -> Either String [ColumnNos]
parseTest4 = parseOnly colNumbersParser

parseTestSH :: ByteString -> IO ()
parseTestSH = parseTest sectionHeadersParser

parseTestWDP :: ByteString -> IO ()
parseTestWDP = parseTest regionWaterDataParser
