{-# LANGUAGE PolyKinds #-}

module Data.Impossible where

import Prelude
import Data.Monoid


-- === Types ===

data Impossible = Impossible deriving (Show)

type ImpossibleM = ImpossibleM1
type ImpossibleT = ImpossibleM2

data ImpossibleM1 t1             = ImpossibleM1 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM2 t1 t2          = ImpossibleM2 deriving (Show)
data ImpossibleM3 t1 t2 t3       = ImpossibleM3 deriving (Show)
data ImpossibleM4 t1 t2 t3 t4    = ImpossibleM4 deriving (Show)
data ImpossibleM5 t1 t2 t3 t4 t5 = ImpossibleM5 deriving (Show)

-- ImpNat is defined as the maximum Int on 64 bit architecture. If GHC has some optimizations of Nat Values, it is nice not to exceed the value.
type ImpossibleNat = 9223372036854775807

-- === Dummy instances === --

instance Applicative ImpossibleM1
instance Monad       ImpossibleM1

-- === Utils === --

impossible :: a
impossible = error "Impossible happened."

impossible' :: String -> a
impossible' = error . ("Impossible happened: " <>)
