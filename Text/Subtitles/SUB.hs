-- |
-- Module      : Text.Subtitles.SUB
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- A basic parser for .sub files (microDVD) based on 'Attoparsec' and 'Text'

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
import Data.Text (pack, Text)
import Data.Maybe (Maybe)

import Text.Subtitles.SUB.Datatypes

-- $example
--
-- I strongly recommend to understand the 'Line' Datatype , which is the
-- foundation of this module.
--
-- Refering to the parts of a single line of a subtitle file.
--
-- >{50}{100}{y:i}Drama here!
--
-- The first to numbers correspond to the frame in which the text Drama here!
-- is displayed. Note that this implementation support the optional flags about
-- text format and color

frame :: Parser Frame
frame = enclosed decimal

-- | Given the example return the corresponding Line representation. At the
-- moment this not handles modifiers as underlines or bold text
parseSingleLine :: Parser Line 
parseSingleLine = Line <$> frame <*> frame <*> parseProperty <*> takeTill isEndOfLine

-- | Main parser of .sub files, given a .sub file it return a list of the dialog
-- lines
parseSUB :: Parser Subtitles
parseSUB = sepBy1 parseSingleLine endOfLine

parseProperty :: Parser (Maybe TextProperty) 
parseProperty = option Nothing (Just <$> enclosed (choice allProp))
  where
    bold      = matchText "y:b" *> pure Bold
    italic    = matchText "y:i" *> pure Italic
    underline = matchText "y:u" *> pure UnderLine
    stroked   = matchText "y:s" *> pure Stroked
    color     = matchText "C:"  *> (C <$> takeWhile1 ('}' /=))
    allProp   = [bold, italic, underline, stroked, color]

-- All .sub command are enclosed in bracket. Worth defining.
enclosed :: Parser a -> Parser a
enclosed p = char '{' *> p <* char '}'

matchText :: String -> Parser Text
matchText s = string (pack s)
