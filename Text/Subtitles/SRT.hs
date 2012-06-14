module Text.Subtitles.SRT where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text 
import qualified Data.Text as T
import Data.Char (isPrint)
import Data.Monoid
import Control.Monad

-- |In_project modules
import Text.Subtitles.Datatypes

parseSubtitles :: Parser Subtitles
parseSubtitles = many1 parseSingleLine

parseSingleLine :: Parser Line
parseSingleLine = 
  let eol = endOfLine 
  in Line <$> decimal <* eol <*> parseRange <* eol <*> parseDialog T.empty

parseRange :: Parser Range
parseRange = Range <$> parseTime <* arrowString <*> parseTime 
  where
    arrowString :: Parser T.Text
    arrowString = string (T.pack " --> ") 

parseTime :: Parser Time
parseTime = Time <$> numDot <*> numDot <*> decimal <* char ',' <*> decimal
  where
    numDot :: Parser Int
    numDot = decimal <* char ':'

parseDialog :: T.Text -> Parser T.Text
parseDialog t = do 
  line <- takeWhile isPrint
  endOfLine
  let lineState = T.append t (T.snoc line '\n')
  next <- anyChar
  case next of
    '\n' -> return lineState 
    _    -> parseDialog (T.snoc lineState next)

