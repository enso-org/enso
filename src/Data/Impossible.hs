{-# LANGUAGE PolyKinds #-}

module Data.Impossible where

import Prelude
import Data.Monoid
import GHC.Stack


-- === Definitions === --

data  Impossible = Impossible deriving (Show)
class ImpCls
type family Imp :: k


-- === Utils === --

impossible :: HasCallStack => a
impossible = withFrozenCallStack $ error "Impossible happened."

impossible' :: HasCallStack => String -> a
impossible' = withFrozenCallStack . error . ("Impossible happened: " <>)


-- === Primitives === --

type instance Imp = Impossible
type instance Imp = ImpCls
type instance Imp = '[Impossible]
type instance Imp = 'Just Imp

type instance Imp = 9223372036854775807 -- ImpNat is defined as the maximum Int on 64 bit architecture. If GHC has some optimizations of Nat Values, it is nice not to exceed the value.
type instance Imp = "*I*M*P*O*S*S*I*B*L*E*"


-- === Parametrized impossibility === --

data ImpossibleM1 t1                         = ImpossibleM1 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM2 t1 t2                      = ImpossibleM2 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM3 t1 t2 t3                   = ImpossibleM3 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM4 t1 t2 t3 t4                = ImpossibleM4 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM5 t1 t2 t3 t4 t5             = ImpossibleM5 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM6 t1 t2 t3 t4 t5 t6          = ImpossibleM6 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM7 t1 t2 t3 t4 t5 t6 t7       = ImpossibleM7 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM8 t1 t2 t3 t4 t5 t6 t7 t8    = ImpossibleM8 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = ImpossibleM9 deriving (Show, Functor, Traversable, Foldable)

type instance Imp = ImpossibleM1
type instance Imp = ImpossibleM2
type instance Imp = ImpossibleM3
type instance Imp = ImpossibleM4
type instance Imp = ImpossibleM5
type instance Imp = ImpossibleM6
type instance Imp = ImpossibleM7
type instance Imp = ImpossibleM8
type instance Imp = ImpossibleM9

-- Dummy instances
instance Applicative ImpossibleM1
instance Monad       ImpossibleM1


-- === Abbreviations === --

type I   = Impossible
type IM  = ImpossibleM1
type IM1 = ImpossibleM1
type IM2 = ImpossibleM2
type IM3 = ImpossibleM3
type IM4 = ImpossibleM4
type IM5 = ImpossibleM5
type IM6 = ImpossibleM6
type IM7 = ImpossibleM7
type IM8 = ImpossibleM8
type IM9 = ImpossibleM9
