module Data.Convert.Errors where

import Prelude
import Data.Typeable

data TypeMismatch = TypeMismatch TypeRep TypeRep deriving (Show)