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
  module Text.Subtitles.SRT.Datatypes,

  -- * Main parsers
  parseSRT,
  parseSingleLine,

  -- * Deprecated
  parseOnly'
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text hiding (parseOnly)
import qualified Data.Attoparsec.Text as DAT
import qualified Data.Text as T
-- in project modules
import Text.Subtitles.SRT.Datatypes

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
  Line <$> parseIndex <*> parseRange <*> optionalGeometry <*> parseDialog []

parseIndex :: Parser Int
parseIndex = decimal <* eol

eol :: Parser ()
eol = endOfLine 

{-# DEPRECATED parseOnly' "AttoParsec's parseOnly was broken before v0.10.2.1" #-}
parseOnly' :: Parser a -> Text -> Either String a
parseOnly' = DAT.parseOnly

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
parseDialog :: [Text] -> Parser Text
parseDialog stateLines = do 
  line <- takeWhile1 (not . isEndOfLine)
  endOfLine
  let stateLines' = line : stateLines
      dialog_complete = return $ T.concat $ reverse stateLines' 
  -- the end of the file
  (endOfInput *> dialog_complete) 
    -- End of this Line, new Line coming.
    <|> (endOfLine *> dialog_complete)
    -- In between lines, the next one belong to this Line,explicit eol required
    <|> parseDialog (T.singleton '\n' : stateLines')
