
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserHelper where
import Debug.Trace 

import Data.Attoparsec.ByteString as Att
import Data.Attoparsec.ByteString.Char8 (endOfLine, char, decimal)
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
-- | parser for the pdf
lFtextTillCRNL :: Parser ByteString
lFtextTillCRNL = endOfLine >> takeWhile (\c -> c /= LF && c /= CR)

textTillCRNL :: Parser ByteString
textTillCRNL = takeWhile (\c -> c /= LF && c /= CR)

colHeaderParser :: Parser ColHeader
colHeaderParser  = string "SR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE (Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. DESIGNED W.R.T. DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n"

colNumbersParser :: Parser [ColumnNos]
colNumbersParser = do 
    x <- decimal `sepBy1` (char ' ')
    endOfLine
    return x

justSectionHeadersParser :: Parser JustSectionHeaders
justSectionHeadersParser  = do
    a <- lFtextTillCRNL
    b <- textTillCRNL
    c <- textTillCRNL
    return $ JustSectionHeaders a b c

sectionHeadersParser :: Parser SectionHeaders
sectionHeadersParser = do
    a <- justSectionHeadersParser
    b <- colHeaderParser
    c <- colNumbersParser
    return $ SectionHeaders a b c

-- | Test 
parseTest0 :: ByteString -> Either String ByteString
parseTest0 = parseOnly lFtextTillCRNL 

parseTest1 :: ByteString -> Either String ByteString
parseTest1 = parseOnly textTillCRNL 

parseTestSH :: ByteString -> Either String SectionHeaders
parseTestSH = parseOnly sectionHeadersParser

parseTest2 :: ByteString -> Either String ByteString
parseTest2 = parseOnly colHeaderParser

parseTest4 :: ByteString -> Either String [ColumnNos]
parseTest4 = parseOnly colNumbersParser