module Data.Index where

import           Prologue


class HasIdx a where idx :: Lens' a (Index a)
