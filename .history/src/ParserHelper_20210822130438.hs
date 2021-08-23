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
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.Word (Word32)
import Prelude hiding (takeWhile)
import WaterLevelHeaders

-- | parser for the pdf
textTillCRNL :: Parser Text
textTillCRNL = do 
    endOfLine
    tgD <- takeWhile (\c -> c /= '\n' && c /= '\r')
    return $ TSE.decodeUtf8 tgD
