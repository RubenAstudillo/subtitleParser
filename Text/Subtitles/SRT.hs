-- |
-- Module      : Text.Subtitles.SRT
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- A basic parser for .srt files (subtitles) based on 'Attoparsec' and 'Text'

module Text.Subtitles.SRT 
  (
  -- * Terminology of the module
  -- $example
  
  -- * Re-exported Datatypes
  module Text.Subtitles.Datatypes,

  -- * Main parsers
  parseSubtitles,
  parseSingleLine
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text 
import qualified Data.Text as T
import Data.Char (isPrint)
import Control.Monad
-- in project modules
import Text.Subtitles.Datatypes

-- $example
--
-- All the sections of a Line have their corresponding ADT in
-- "Text.Subtitles.Datatypes"
-- 
-- >2
-- >00:00:50,050 --> 00:00:52,217
-- >Drama here
--
-- The whole Line is represented in the 'Line' ADT which constructors
-- represented by different ADTs
--
-- * The first line is called index, which is the first constructor of
--   'Line'.
--
-- * The second one is called 'Range', which correspond to two separated 'Time'.
--
-- * The last one is the 'subs'. which is just Text and correspond to the third
--   constructor of 'Line'.

-- |Main Parser, gives you a list of all the Lines of the subtitle. It fails if
--  the subtitle doesn't have any Lines.
parseSubtitles :: Parser Subtitles
parseSubtitles = many1 parseSingleLine

-- |The individual Line parser. given the upper example return the
-- corresponding Line representation
parseSingleLine :: Parser Line
parseSingleLine = 
  Line <$> parseIndex <*> parseRange <* eol <*> parseSubs T.empty

parseIndex :: Parser Int
parseIndex = decimal <* eol

eol :: Parser ()
eol = endOfLine 

{- Is clear that this just aplies parseTime breaking down the "-->" string -}
parseRange :: Parser Range
parseRange = Range <$> parseTime <* arrowString <*> parseTime 
  where
    arrowString :: Parser Text
    arrowString = string (T.pack " --> ") 

parseTime :: Parser Time
parseTime = Time <$> numDot <*> numDot <*> decimal <* char ',' <*> decimal
  where
    numDot :: Parser Int
    numDot = decimal <* char ':'

{- return the dialog checking for newlines that could be in there. that why is
 - written in a monad instead of applicative. more efficient version welcome -}
parseSubs :: Text -> Parser Text
parseSubs t = do 
  line <- takeWhile1 (not . isEndOfLine)
  endOfLine
  let lineState = T.append t (T.snoc line '\n')
  next <- anyChar
  case next of
    '\n' -> return lineState 
    _    -> parseSubs (T.snoc lineState next)

