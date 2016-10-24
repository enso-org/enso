{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Layout.Nested where

import Luna.Syntax.Term.Expr.Atom
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Layout.Class

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
