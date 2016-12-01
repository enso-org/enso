{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.ENT where

import Luna.Prelude hiding (Simple)

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
type instance DefaultLayout Ent = ENT () () Star

-- Init layouts
type instance LiteralLayout p (ENT a n t) = ENT p () Star
type instance AtomLayout    p (ENT a n t) = ENT p () Star

-- Sub
type instance Sub Atom (ENT a n t) = ENT (Sub Atom a) n t
type instance Sub Name (ENT a n t) = ENT (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ENT a n t) = ENT (Sub Type t) (Sub Type t) (Sub Type t)

type instance AsSubLayout Name (ENT a n t) = a <+> n

-- Specialized
type instance Specialized Atom s (ENT a n t) = ENT (Simplify (AsSubLayout Atom s :> a)) n t
type instance Specialized Name s (ENT a n t) = ENT a (Simplify (AsSubLayout Name s :> n)) t
type instance Specialized Type s (ENT a n t) = ENT a n (Simplify (AsSubLayout Type s :> t))

-- Generalize
instance (Generalize e e', Generalize n n', Generalize t t') => Generalize (ENT e n t) (ENT e' n' t')


---- REFACTOR:

-- To powinno byc generalizowanie per layout:
instance Generalize () (Form f)






-- to powinno byc w class ale jest jeszcze cykl Class -> Type -> Ir -> Class
type l |>  r = Specialized   Atom l r
type l #>  r = Specialized   Name l r
type l >>  r = Specialized   Type l r
