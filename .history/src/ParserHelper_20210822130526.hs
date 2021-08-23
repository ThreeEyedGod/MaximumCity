{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ParserHelper where

import Data.Attoparsec.ByteString as Att
import qualified Data.Word8 as W
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString ( ByteString )
import Control.Monad
import qualified Data.ListLike as LL
import Data.Char
import Prelude hiding (takeWhile)
import WaterLevelHeaders

-- | parser for the pdf
textTillCRNL :: Parser Text
textTillCRNL = do 
    endOfLine
    tgD <- takeWhile (\c -> c /= '\n' && c /= '\r')
    return $ TSE.decodeUtf8 tgD