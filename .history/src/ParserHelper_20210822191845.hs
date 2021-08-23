
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserHelper where

import Data.Attoparsec.ByteString as Att
import Data.Attoparsec.ByteString.Char8 (endOfLine)
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
textTillCRNL :: Parser ByteString
textTillCRNL = endOfLine >> takeWhile (\c -> c /= LF && c /= CR)

sectionHeadersParser :: Parser SectionHeaders
sectionHeadersParser = 
    
-- | Test 
parseTest :: ByteString -> Either String ByteString
parseTest = parseOnly textTillCRNL 