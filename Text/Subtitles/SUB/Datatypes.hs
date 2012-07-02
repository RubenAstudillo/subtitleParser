-- |
-- Module      : Text.Subtitles.SUB.Datatypes
-- Copyright   : Ruben Astudillo 2012
-- License     : BSD3
--
-- Maintainer  : ruben.astud@gmail.com
-- Portability : unknown
--
-- ADT for .sub files. Also serves as a place to  provide instance 
-- declarations for the ADTs.

module Text.Subtitles.SUB.Datatypes 
  (
  -- * Re-exported datatypes
  module Data.Attoparsec.Text,
  module Data.Text,
  
  -- * Datatypes
  Frame,
  Subtitles,
  Line(..)
  ) where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser)

type Subtitles = [Line]

{- Should this be a newtype? or just a type? -}
type Frame = Int

data Line = Line { startFrame :: Frame,
                   finalFrame :: Frame,
                   dialog     :: Text }
  deriving (Show)

