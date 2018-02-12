{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout where

import Prologue



infixr 7 ^.
type family src ^. prop


----------------------------
-- === Type level map === --
----------------------------

type Map k v = [(k, v)]
type a := b = '(a, b)

type family Get (key :: k) (map :: Map k v) :: v where
    Get k ((k := v) ': _) = v
    Get k ((l := v) ': t) = Get k t



--------------------
-- === Layout === --
--------------------


data Layout
    = Nested Type (Map Type Layout)
    | Flat   Type

type a -| b = 'Nested a b


type family Base (l :: Layout) where
    Base (Nested a _) = a
    Base (Flat   a)   = a

type family Branches (l :: Layout) where
    Branches (Nested _ t) = t
    Branches (Flat _)     = '[]

type family Rebase (a :: Type) (l :: Layout) where
    Rebase a (Nested _ t) = Nested a t
    Rebase a (Flat _)     = Flat   a

type family   SubLayout (t :: Type) (layout :: l) :: Layout
type instance SubLayout t (Nested _ m) = Get t m


-- type family Foo (a :: k) :: Nat

-- type instance Foo 1 = 2
-- type instance Foo "a" = 1

-- Draft :>
