module Data.Graph.Data.Component.List where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component



------------------
-- === List === --
------------------

-- === Definition === --

type List' = List ()
data List l
    = Cons !(Component.Some l) !(List l)
    | Nil
    deriving Show


-- === Helpers === --

type family Lists ls where
    Lists '[]       = '[]
    Lists (l ': ls) = List l ': Lists ls


-- === Instances === --

instance Mempty  (List l) where mempty = Nil    ; {-# INLINE mempty #-}
instance Default (List l) where def    = mempty ; {-# INLINE def    #-}
