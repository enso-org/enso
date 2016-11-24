{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Layout.Nested where

import Luna.IR.Term.Atom
import Luna.IR.Term.Format
import Luna.IR.Term.Layout.Class

import Type.Bool


---------------------------
-- === Nested layout === --
---------------------------

-- === Definition === --

data a :> b


-- === Instances === --

type instance Sub t (a :> b) = b
type instance Atoms (a :> b) = Atoms a

-- Generalize
instance {-# OVERLAPPABLE #-} (Generalize a b, Generalize sa sb)        => Generalize (a :> sa) (b :> sb)
instance {-# OVERLAPPABLE #-} (Generalize (a :> sa) (Form b :> Form b)) => Generalize (a :> sa) (Form b)

-- Simplify
type instance Simplify (a :> b) = If (a == b) a (a :> b)

-- Merge
type instance Merge (t :> s)   ()         = t :> s
type instance Merge ()         (t :> s)   = t :> s
type instance Merge ()         ()         = ()
type instance Merge (t :> s)   (t' :> s') = Simplify (Merge t t' :> Merge s s')
type instance Merge (t :> s)   (Atomic a) = Simplify (Merge t (Atomic a)   :> Merge s (Atomic a))
type instance Merge (t :> s)   (Form a)   = Simplify (Merge t (Form   a)   :> Merge s (Form   a))
type instance Merge (Atomic a) (t :> s)   = Simplify (Merge   (Atomic a) t :> Merge   (Atomic a) s)
type instance Merge (Form a)   (t :> s)   = Simplify (Merge   (Form   a) t :> Merge   (Form   a) s)
