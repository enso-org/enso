{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.Nested where

import Luna.IR.Expr.Atom
import Luna.IR.Expr.Format
import Luna.IR.Expr.Layout.Class

import Type.Bool


---------------------------
-- === Nested layout === --
---------------------------

-- === Definition === --

infixr 7 >>>
data a >>> b

-- type a >>> b = Simplify (a >> b)

-- === Instances === --

type instance Sub t (a >>> b) = Sub t b
type instance Atoms (a >>> b) = Atoms a


type instance Generalizable (a >>> sa) (b >>> sb) = Generalizable a b && Generalizable sa sb
type instance Generalizable (a >>> sa) (Form b)  = Generalizable (a >>> sa) (Form b >>> Form b)

-- Simplify
-- type instance Simplify (a >> b) = If (a == b) a (a >> b)

type instance Merge (l >>> r)   Bottom        = Bottom
type instance Merge Bottom        (l >>> r)   = Bottom

-- Merge
-- type instance Merge (t >> s)   ()         = t >> s
-- type instance Merge ()         (t >> s)   = t >> s
-- type instance Merge ()         ()         = ()
-- type instance Merge (t >> s)   (t' >> s') = Simplify (Merge t t' >> Merge s s')
-- type instance Merge (t >> s)   (Atomic a) = Simplify (Merge t (Atomic a)   >> Merge s (Atomic a))
-- type instance Merge (t >> s)   (Form a)   = Simplify (Merge t (Form   a)   >> Merge s (Form   a))
-- type instance Merge (Atomic a) (t >> s)   = Simplify (Merge   (Atomic a) t >> Merge   (Atomic a) s)
-- type instance Merge (Form a)   (t >> s)   = Simplify (Merge   (Form   a) t >> Merge   (Form   a) s)


--- REFACTOR:

-- type ENT  l a n t = Compound l '[Atom := a, Name := n, Type := t]

type instance Simplify (a >>> ()) = a

type instance Current (a >>> b) = a
