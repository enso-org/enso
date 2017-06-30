{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout.Nested where

import OCI.IR.Term
import OCI.IR.Layout.Class

import Type.Bool


---------------------------
-- === Nested layout === --
---------------------------

-- === Definition === --

infixr 7 >>>
data a >>> b


-- === Instances === --

type instance Sub t (a >>> b) = If ((b ^ t) == Fail) (a ^ t) (b ^ t)
type instance TermTypesOf (a >>> b) = TermTypesOf a

type instance Generalizable (a >>> sa) (b >>> sb) = Generalizable a b && Generalizable sa sb

type instance Merge (l >>> r) Bottom    = Bottom
type instance Merge Bottom    (l >>> r) = Bottom

type instance Simplify (a >>> ()) = a
type instance Current  (a >>> b)  = a
