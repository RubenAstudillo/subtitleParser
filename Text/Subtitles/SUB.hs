-- |
-- Module      : Text.Subtitles.SUB
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- A basic parser for .sub files (subtitles) based on 'Attoparsec' and 'Text'

module Text.Subtitles.SUB 
  (
  parseSingleLine,
  parseSUB 
  ) where

import Control.Applicative
import Data.Attoparsec.Text

import Text.Subtitles.SUB.Datatypes

frame :: Parser Frame
frame = char '{' *> decimal <* char '}'

parseSingleLine :: Parser Line 
parseSingleLine = Line <$> frame <*> frame <*> takeTill isEndOfLine

parseSUB :: Parser Subtitles
parseSUB = sepBy1 parseSingleLine endOfLine
