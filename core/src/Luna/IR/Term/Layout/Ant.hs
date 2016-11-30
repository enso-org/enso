module Luna.IR.Term.Layout.Ant where

import Luna.Prelude hiding (Simple)

import Luna.IR.Term.Layout.Class
import Luna.IR.Term.Layout.Compound
import Luna.IR.Term.Layout.Nested
import Luna.IR.Term.Format
import Luna.IR.Term.Atom
import Luna.IR.Layer.Type (Type)
import Data.RTuple        (Assoc ((:=)))


-----------------
-- === ANT === --
-----------------

-- === Definition === ---

data ANT
data Ant a n t
-- type Ant  l a n t = Compound l '[Atom := a, Name := n, Type := t]
-- type Ant'   a n t = Ant Simple a n t


-- === Simple Ant layout === --

-- data Simple

-- DefaultLayout
type instance DefaultLayout ANT = Ant () () Star

-- Sub
type instance Sub Atom (Ant a n t) = Ant (Sub Atom a) n t
type instance Sub Name (Ant a n t) = Ant (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (Ant a n t) = Ant (Sub Type t) (Sub Type t) (Sub Type t)
