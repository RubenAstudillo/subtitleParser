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

module Text.Subtitles.Datatypes where

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
  show (Time h m s f) = concat [show h, ":", show m, ":", show s, ",", show f]

instance Show Range where
  show (Range f t) = concat [show f, " --> ", show t]

instance Show Line where
  show (Line i t s) = intercalate "\n" (show i : show t : [unpack s]) ++ "\n"
