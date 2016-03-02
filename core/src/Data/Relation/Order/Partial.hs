module Data.Relation.Order.Partial where

import Prelude


-- === Definitions === ---


data Strict    a = LowerThan a deriving (Show, Functor, Foldable, Traversable)
data NonStrict a = EqualTo   a deriving (Show, Functor, Foldable, Traversable)
