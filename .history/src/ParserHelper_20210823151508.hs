
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserHelper where
import Debug.Trace 

import Data.Attoparsec.ByteString as Att
import Data.Attoparsec.ByteString.Char8 (isDigit_w8, manyTill, isHorizontalSpace, endOfLine, char, decimal, space, double, letter_ascii)
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

percentage :: Parser ()
percentage =  void (char '%') <?> "end of line"

-- | parser for the pdf

_takeWhile ::

lFtextTillCRNL :: Parser ByteString
lFtextTillCRNL = endOfLine >> textTillCRNL

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

-- "\n1 Amravati 10 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%\n2 Aurangabad 45 1444.86 4505.36 5950.21 2283.74 3716.54 50.69% 62.14%\n3 Kokan 11 123.37 2459.4 2582.77 2132.26 2248.38 86.7% 79.21%\n4 Nagpur 16 839.5 3462.92 4302.42 1766.13 2605.63 51.0% 70.85%\n5 Nashik 24 529.18 3738.75 4267.93 2238.47 2767.63 59.87% 66.87%\n6 Pune 35 2950.74 12444.02 15394.75 10267.09 13220.26 82.51% 72.67%\nTotal Major Projects"
categoryProjectsLinedataParser :: Parser [RegionEntry]
categoryProjectsLinedataParser = manyTill regionEntryParser (string "\nTotal Major Projects")

my_isAlpha_ascii :: Word8 -> Bool
my_isAlpha_ascii c = not $ isDigit_w8 c

-- | \nTotal Major Projects 141 6477.59 28974.36 35451.96 20186.96 26670.32 69.67% 70.94% 
totalCategoryProjectsParser :: Parser TotalProjectFooter
totalCategoryProjectsParser = do
    endOfLine
    a <- takeWhile my_isAlpha_ascii 
    --space which is also an alpha i.e not a digit !
    b <- decimal
    space
    c <- regionWaterDataParser
    return $ TotalProjectFooter a b c

-- | "\nStatus as on Date: 18 Aug 2021 \nMajor, Medium and Minor Projects Live Storage Comparison\nRevenue Region\nSR. SR.\nNO. NO.\nNO. OF NO. OF\nDAMS DAMS\nDESIGNED STORAGE DESIGNED STORAGE (Mcum) (Mcum)\nTODAY'S LIVE TODAY'S LIVE\nSTORAGE STORAGE (Mcum) (Mcum)\nPERCENTAGE OF PERCENTAGE OF\nLIVE STORAGE LIVE STORAGE\nW.R.T. DESIGNED W.R.T. DESIGNED\nLIVE STORAGE LIVE STORAGE\nDEAD DEAD LIVE LIVE GROSS GROSS LIVE LIVE GROSS GROSS\nFOR FOR\nTODAY TODAY\nSAME SAME\nDATE DATE\nOF LAST OF LAST\nYEAR YEAR\n1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10\nMajor Projects\n1 Amravati 10 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%\n2 Aurangabad 45 1444.86 4505.36 5950.21 2283.74 3716.54 50.69% 62.14%\n3 Kokan 11 123.37 2459.4 2582.77 2132.26 2248.38 86.7% 79.21%\n4 Nagpur 16 839.5 3462.92 4302.42 1766.13 2605.63 51.0% 70.85%\n5 Nashik 24 529.18 3738.75 4267.93 2238.47 2767.63 59.87% 66.87%\n6 Pune 35 2950.74 12444.02 15394.75 10267.09 13220.26 82.51% 72.67%\nTotal Major Projects 141 6477.59 28974.36 35451.96 20186.96 26670.32 69.67% 70.94%
categoryProjectsParser :: Parser CategoryProjects
categoryProjectsParser = do
    a <- categoryHeaderParser
    b <- categoryProjectsLinedataParser
    c <- totalCategoryProjectsParser 
    return $ CategoryProjects a b c 

-- | 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%
regionWaterDataParser :: Parser RegionWaterData
regionWaterDataParser = do
    a <- double 
    space
    b <- double
    space
    c <- double
    space
    d <- double
    space
    e <- double
    space
    f <- textTillPCT
    percentage  >> space
    g <- textTillPCT
    --percentage >> endOfLine
    percentage
    return $ RegionWaterData a b c d e f g 

-- | \n1 Amravati 10 589.95 2363.93 2953.88 1499.27 2111.88 63.42% 76.51%\n
regionEntryParser :: Parser RegionEntry
regionEntryParser = do
    endOfLine
    a <- decimal
    space 
    b <- takeTill isHorizontalSpace
    space
    c <- decimal
    space
    d <- regionWaterDataParser
    return $ RegionEntry a b c d

-- | Tests
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

parseTestRE :: ByteString -> IO ()
parseTestRE = parseTest regionEntryParser

parseTestWDP :: ByteString -> IO ()
parseTestWDP = parseTest regionWaterDataParser

parseTestCPS :: ByteString -> IO ()
parseTestCPS = parseTest categoryProjectsParser

parseTestTPFooter :: ByteString -> IO ()
parseTestTPFooter = parseTest totalCategoryProjectsParser

parseTestCPP :: ByteString -> IO ()
parseTestCPP = parseTest categoryProjectsParser