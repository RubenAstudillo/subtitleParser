-- |
-- Module      : Text.Subtitles.SRT.Datatypes
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- ADT for .srt files. Also serves as a place to  provide instance
-- declarations for the ADTs.

module Text.Subtitles.SRT.Datatypes 
  (
  -- * Re-exported datatypes.
  module Data.Attoparsec.Text,
  module Data.Text,

  -- * Datatypes
  Subtitles,
  Line(..),
  Range(..),
  Time(..)
  ) where

import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.Attoparsec.Text (Parser)

data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , frame   :: Int
  } deriving (Eq, Ord, Show)

data Range = Range 
  { from :: Time
  , to   :: Time
  } deriving (Eq, Ord, Show)

-- | The core of the parser. each one of the constructor representing one part
-- of the Line
data Line = Line
  { index  :: Int
  , range  :: Range
  , dialog   :: Text
  } deriving (Eq, Ord, Show)

type Subtitles = [Line]

