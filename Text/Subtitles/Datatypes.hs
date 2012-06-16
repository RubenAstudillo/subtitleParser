-- |
-- Module      : Text.Subtitles.Datatypes
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- Common ADT for the project. Also serves as a place to  provide instance
-- declaration for the ADTs.

module Text.Subtitles.Datatypes 
  (
  -- * Datatypes
  Subtitles(..),
  Line(..),
  Range(..),
  Time(..)
  ) where

import Data.List (intercalate)
import Data.Text (Text, unpack)

data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , frame   :: Int
  } deriving (Eq, Ord)

data Range = Range 
  { from :: Time
  , to   :: Time
  } deriving (Eq, Ord)

-- | The core of the parser. each one of the constructor representing one part
-- of the Line
data Line = Line
  { index  :: Int
  , range  :: Range
  , subs   :: Text
  } deriving (Eq, Ord)

type Subtitles = [Line]

instance Show Time where
  show (Time h m s f) = concat [showT h, ":", showT m, ":", showT s, ",", showF f]

instance Show Range where
  show (Range f t) = concat [show f, " --> ", show t]

instance Show Line where
  show (Line i t s) = intercalate "\n" (show i : show t : [unpack s]) ++ "\n"

{- showT stands for showTime. In the parsing process "00" is read as "0", and
 - that tailing 0 is lost unless we add it manually when showing -}
showT :: Int -> String
showT a | a < 10    = '0' : show a
        | otherwise = show a

{- showF stands for showFrame -}
showF :: Int -> String
showF a | a < 10    = '0':'0': show a
        | a < 100   = '0': show a
        | otherwise = show a
