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
  -- * Don't use parseOnly!
  -- $noParseOnly 
  
  -- * Terminology of the module
  -- $example
  
  -- * Re-exported Datatypes
  module Text.Subtitles.SRT.Datatypes,

  -- * Main parsers
  parseSRT,
  parseSingleLine,

  -- * Temporal solutions
  parseOnly'
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text hiding (parseOnly)
import qualified Data.Text as T
-- in project modules
import Text.Subtitles.SRT.Datatypes

-- $noParseOnly
--
-- This module uses now peekChar in parseDialog, which replaces the ad-hoc
-- method used before. As a consequence it doesn't play well with
-- Data.Attoparsec.Text.parseOnly on some conditions.
--
-- You should use just 'parse' or the parseOnly' function I provide to avoid
-- problems until further notice. /Hopefully/ in the next version of attoparsec
-- this will be solved , so keep an eye for removal!.

-- $example
--
-- All the sections of a Line have their corresponding ADT in
-- "Text.Subtitles.SRT.Datatypes"
-- 
-- >2
-- >00:00:50,050 --> 00:00:52,217 X1:1 X2:2 Y1:1 Y2:2
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
-- * After the range is an optional field called Rectangle which says what
--   geometry should the text obey.
--
-- * The last one is the 'subs'. Which is just Text and correspond to the third
--   constructor of 'Line'.

-- |Main Parser, gives you a list of all the Lines of the subtitle. It fails if
--  the subtitle doesn't have any Lines.
parseSRT :: Parser Subtitles
parseSRT = many1 parseSingleLine

-- |The individual Line parser. Given the upper example return the
-- corresponding Line representation
parseSingleLine :: Parser Line
parseSingleLine = 
  Line <$> parseIndex <*> parseRange <*> optionalGeometry <*> parseDialog T.empty

parseIndex :: Parser Int
parseIndex = decimal <* eol

eol :: Parser ()
eol = endOfLine 

-- |This version avoid the problems associated with peekChar and thus is safe to
-- use in this module. Subject to removal once parseOnly is fixed.
parseOnly' :: Parser a -> Text -> Either String a
parseOnly' p t = eitherResult $ feed (parse p t) T.empty

{- Is clear that this just aplies parseTime breaking down the "-->" string -}
parseRange :: Parser Range
parseRange = Range <$> parseTime <* arrowString <*> parseTime <* skipSpace
  where
    arrowString :: Parser Text
    arrowString = string (T.pack " --> ") 

{- the order X1 X2 Y1 Y2 seems to be enforced, so we can check only for order
 - instead of keywords -}
optionalGeometry :: Parser (Maybe Rectangle)
optionalGeometry = option Nothing (Just <$> rectangle <* eol)
  where rectangle = R <$> valueSpace <*> valueSpace <*> valueSpace <*> value
        value   = letter *> digit *> char ':' *> decimal 
        valueSpace = value <* space

parseTime :: Parser Time
parseTime = Time <$> numDot <*> numDot <*> decimal <* char ',' <*> decimal
  where
    numDot :: Parser Int
    numDot = decimal <* char ':'

{- return the dialog checking for newlines that could be in there. That why is
 - written in a monad instead of applicative. More efficient version welcome -}
parseDialog :: Text -> Parser Text
parseDialog stateLine = do 
  line <- takeWhile1 (not . isEndOfLine)
  endOfLine
  let stateCurrent = T.append stateLine line
      lineState = T.snoc stateCurrent '\n' --takeWhile1 didn't consume \n
  next <- peekChar
  case next of
    Nothing     -> return stateCurrent  -- the end of the file
    (Just '\n') -> eol >> return stateCurrent -- End of this Line, new Line coming.
    (Just _)    -> parseDialog lineState {- in between lines, the next one belong to this Line
                                         explicit eol required -}


