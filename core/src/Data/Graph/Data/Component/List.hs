module Data.Graph.Data.Component.List where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Graph.Data.Component.Class (Component)



------------------
-- === List === --
------------------

-- === Definition === --

type List' = List ()
data List comp
    = Cons !(Component.Some comp) !(List comp)
    | Nil
    deriving Show


-- === Helpers === --

type family Lists ls where
    Lists '[]       = '[]
    Lists (l ': ls) = List l ': Lists ls


-- === Instances === --

type instance Item (List comp) = Component.Some comp

instance Mempty  (List comp) where mempty = Nil    ; {-# INLINE mempty #-}
instance Default (List comp) where def    = mempty ; {-# INLINE def    #-}

instance comp ~ comp'
      => Convertible [Component comp layout] (List comp') where
    convert = \case
        []     -> Nil
        (a:as) -> Cons (Layout.relayout a) $! convert as
    {-# INLINABLE convert #-}

instance comp ~ comp'
      => Convertible (List comp') [Component.Some comp] where
    convert = \case
        Nil       -> []
        Cons a as -> let as' = convert as in a : as'
    {-# INLINABLE convert #-}

instance Semigroup (List comp) where
    l <> l' = case l of
        Nil       -> l'
        Cons a as -> Cons a (as <> l')
    {-# INLINABLE (<>) #-}
