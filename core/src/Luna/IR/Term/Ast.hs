module Luna.IR.Term.Ast where

import Prologue hiding (seq)

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
    data Invalid      = Invalid      { desc :: Name }
    data Marked       = Marked       { marker :: LinkTo Terms, body :: LinkTo Terms }
    data Marker       = Marker       { id :: Word64 }
    data SectionLeft  = SectionLeft  { operator :: LinkTo Terms, body :: LinkTo Terms }
    data SectionRight = SectionRight { operator :: LinkTo Terms, body :: LinkTo Terms }
    data Seq          = Seq          { former :: LinkTo Terms, later :: LinkTo Terms }
    |]

