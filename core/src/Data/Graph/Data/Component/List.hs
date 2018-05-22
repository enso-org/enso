module Data.Graph.Data.Component.List where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component



------------------
-- === List === --
------------------

-- === Definition === --

data List l
    = Cons !(Component.Some l) !(List l)
    | Nil
    deriving Show


-- === Helpers === --

type family Lists ls where
    Lists '[]       = '[]
    Lists (l ': ls) = List l ': Lists ls


-- === Instances === --

instance Default (List l) where
    def = Nil ; {-# INLINE def #-}
