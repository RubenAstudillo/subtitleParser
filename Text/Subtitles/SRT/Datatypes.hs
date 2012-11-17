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
  Time(..),
  Rectangle(..)
  ) where

import Data.Text (Text, unpack)
import Data.Attoparsec.Text (Parser)

-- |This represent the position on screen of the Line. Is usually optional in
-- the file.
data Rectangle = R { x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int}
  deriving (Eq, Ord, Show)

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
  { index  :: Int -- ^The absolute order of this line.
  , range  :: Range -- ^The interval of time that the line is shown.
  , geometry :: Maybe Rectangle  -- ^Sometimes text shouldn't be on the lower center.
  , dialog   :: Text -- ^what to show in screen
  } deriving (Eq, Ord, Show)

-- |A subtitle is just a List of independent Lines that appear on screen
type Subtitles = [Line]

