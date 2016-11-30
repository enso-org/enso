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
-- | The Expression Name Type (ENT) Layout.


-- === Definition === ---

data Ent
data ENT a n t


-- === Simple ENT layout === --

-- DefaultLayout
type instance DefaultLayout Ent = ENT () () Star

-- Sub
type instance Sub Atom (ENT a n t) = ENT (Sub Atom a) n t
type instance Sub Name (ENT a n t) = ENT (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ENT a n t) = ENT (Sub Type t) (Sub Type t) (Sub Type t)
