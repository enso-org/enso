{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Selector where

import Prologue

import Type.Data.List (DynTreeList)

import qualified Type.Data.List as List


-----------------------
-- === Selectors === --
-----------------------

-- === Definition === --

data Selector (l :: kl) (r :: kr)
type (/) = Selector


-- === Expand === --

type family Expand (s :: k) :: [[Type]] where
    Expand s = ExpandAfter__ (Expand__ s)

type family ExpandAfter__ (lsts :: [Type]) :: [[Type]] where
    ExpandAfter__ '[]       = '[]
    ExpandAfter__ (l ': ls) = List.FlattenDynTreeList l ': ExpandAfter__ ls

type family Expand__ (s :: k) :: [Type] where
    Expand__ (Selector base child) = List.CartesianWith DynTreeList
                                     (Expand__ base) (Expand__ child)
    Expand__ (a :: [Type])         = ExpandEvery__ a
    Expand__ (a :: Type)           = '[a]

type family ExpandEvery__ (s :: [k]) :: [Type] where
    ExpandEvery__ '[] = '[]
    ExpandEvery__ (s ': ss) = List.Append (Expand__ s) (ExpandEvery__ ss)
