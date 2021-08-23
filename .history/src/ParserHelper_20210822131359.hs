
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserHelper where

import Data.Attoparsec.ByteString as Att
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
textTillCRNL = takeWhile (\c -> c /= LF && c /= '\r')
