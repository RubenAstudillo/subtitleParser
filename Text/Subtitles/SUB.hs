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
  -- * Terminology of the module
  -- $example 

  -- *Re-exported Datatypes
  module Text.Subtitles.SUB.Datatypes,
  
  -- * Main Parsers
  parseSingleLine,
  parseSUB 
  ) where

import Control.Applicative
import Data.Attoparsec.Text

import Text.Subtitles.SUB.Datatypes

-- $example
--
-- Refering to the parts of a single line of a subtitle file.
--
-- >{50}{100}Drama here!
--
-- the first to numbers correspond to the frame in which the text Drama here!
-- is displayed. 

frame :: Parser Frame
frame = char '{' *> decimal <* char '}'

-- | Given the example return the corresponding Line representation. At the
-- moment this not handles modifiers as underlines or bold text
parseSingleLine :: Parser Line 
parseSingleLine = Line <$> frame <*> frame <*> takeTill isEndOfLine

-- | Main parser of .sub files, given a .sub file it return a list of the dialog
-- lines
parseSUB :: Parser Subtitles
parseSUB = sepBy1 parseSingleLine endOfLine
