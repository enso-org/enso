{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.ENT where

import Luna.Prelude hiding (Simple, String)

import Luna.IR.Expr.Layout.Class
import Luna.IR.Expr.Layout.Nested
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom
import Luna.IR.Layer.Type (Type)
import Data.RTuple        (Assoc ((:=)))


-----------------
-- === Ent === --
-----------------

-- === Definition === ---

data Ent
data ENT a n t


-- === Simple ENT layout === --

-- DefaultLayout
type instance DefaultLayout Ent = ENT () String Star

-- Init layouts
type instance LiteralLayout p (ENT a n t) = ENT p String Star
type instance AtomLayout    p (ENT a n t) = ENT p String Star

-- Sub
type instance Sub Atom (ENT a n t) = ENT (Sub Atom a) n t
type instance Sub Name (ENT a n t) = Sub Name n
type instance Sub Type (ENT a n t) = Sub Type t

type instance AsSubLayout Name (ENT a n t) = a <+> n

-- Specialized
type instance Specialized Atom (ENT a' n' t') (ENT a n t) = ENT (a' :>> a) (n' <+> n) (t' <+> t)
type instance Specialized Name (ENT a' n' t') (ENT a n t) = ENT a (Specialized Atom (ENT a' n' t') n) t
type instance Specialized Type (ENT a' n' t') (ENT a n t) = ENT a n (Specialized Atom (ENT a' n' t') t)

type instance Specialized Atom (ENT e n t) () = ENT e n t
-- type instance Specialized Atom s (ENT a n t) = ENT (AsSubLayout Atom s :>> a) n t
-- type instance Specialized Name s (ENT a n t) = ENT a (AsSubLayout Name s :>> n) t
-- type instance Specialized Type s (ENT a n t) = ENT a n (AsSubLayout Type s :>> t)

-- Generalize
instance (Generalize e e', Generalize n n', Generalize t t') => Generalize (ENT e n t) (ENT e' n' t')


---- REFACTOR:

-- To powinno byc generalizowanie per layout:
instance Generalize () (Atomic a)
instance Generalize () (Form   f)




type instance Merge (ENT e n t) (ENT e' n' t') = ENT (Merge e e') (Merge n n') (Merge t t')

-- to powinno byc w class ale jest jeszcze cykl Class -> Type -> Ir -> Class
type l |> r = Specialized Atom l r
type l #> r = Specialized Name l r
type l >> r = Specialized Type l r
