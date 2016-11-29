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
type Ant  l a n t = Compound l '[Atom := a, Name := n, Type := t]
type Ant'   a n t = Ant Simple a n t


-- === Simple Ant layout === --

data Simple

-- DefaultLayout
type instance DefaultLayout ANT = Ant Simple () () Star

-- Sub
type instance Sub Atom (Ant Simple a n t) = Ant Simple (Sub Atom a) n t
type instance Sub Name (Ant Simple a n t) = Ant Simple (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (Ant Simple a n t) = Ant Simple (Sub Type t) (Sub Type t) (Sub Type t)

-- Specialized
type instance Specialized Atom spec (Ant l a n t) = Ant l (Simplify (spec :> a)) n t
