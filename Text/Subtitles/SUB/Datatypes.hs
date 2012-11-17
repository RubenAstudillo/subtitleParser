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
  Subtitles,
  Line(..),
  TextProperty(..),
  Frame,
  Color
  ) where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import Data.Maybe (Maybe)

type Subtitles = [Line]

{- Should this be a newtype? or just a type? -}
type Frame = Int

-- .sub utilize a non-standard RGB format, is better to keep them as Text 
type Color = Text

-- |Optional property of text, the constructor should be clear.
data TextProperty = Italic | Bold | UnderLine | Stroked 
                  | C Color -- ^ This is just text because is non-standard RGB
  deriving (Show, Eq)

data Line = Line { startFrame :: Frame,
                   finalFrame :: Frame,
                   property   :: Maybe TextProperty, 
                   dialog     :: Text }
  deriving (Show, Eq)

