module Data.Convert.Errors where

import Data.Typeable

data TypeMismatch = TypeMismatch TypeRep TypeRep deriving (Show)