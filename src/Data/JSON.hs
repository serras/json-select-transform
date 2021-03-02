module Data.JSON where

import           Data.Text (Text)

-- | A position within a document.
type Path = [PathElement]

-- | Each of the steps making up a 'Path'.
data PathElement
  = PathElementKey Text
  | PathElementIndex Int
  deriving (Eq, Show)
