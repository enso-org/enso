module Luna.IR.Term.Ast where

import Prologue

import qualified Luna.IR.Component.Term.Definition as Term
import qualified Luna.IR.Term.Format               as Format

import Data.Vector.Storable.Foreign      (Vector)
import Luna.IR.Component.Term.Class      (Terms)
import Luna.IR.Component.Term.Definition (LinkTo)
import OCI.Data.Name                     (Name)

-- FIXME: remove when refactoring Cmp instances
import Luna.IR.Term.Core ()


--------------------
-- === Number === --
--------------------

-- === Definition === --

Term.define ''Format.Ast [d|
    data Invalid a = Invalid { desc   :: Name                                   }
    data Marked  a = Marked  { marker :: LinkTo Terms a, body :: LinkTo Terms a }
    data Marker  a = Marker  { id     :: Word64                                 }
    |]

