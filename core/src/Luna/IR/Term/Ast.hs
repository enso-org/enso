{-# LANGUAGE CPP #-}

module Luna.IR.Term.Ast where

import Prologue hiding (seq)

import qualified Luna.IR.Component.Term.Definition as Term
import qualified Luna.IR.Term.Format               as Format

import Data.Vector.Storable.Foreign      (Vector)
import Luna.IR.Component.Term.Class      (Terms)
import Luna.IR.Component.Term.Definition (LinkTo, Ln)
import OCI.Data.Name                     (Name)

-- FIXME: remove when refactoring Cmp instances
import Luna.IR.Term.Core ()

import Data.PtrList.Mutable (UnmanagedPtrList)
type LinkListTox a = UnmanagedPtrList (LinkTo a)

--------------------
-- === Number === --
--------------------

-- === Definition === --

Term.define [d|
 data Ast
    = AccSection   { path     :: Vector Name                                   }
    | Disabled     { body     :: LinkTo Terms                                  }
    | Documented   { doc      :: Vector Char  , base  :: LinkTo Terms          }
    | Grouped      { body     :: LinkTo Terms                                  }
    | Invalid      { desc     :: Name                                          }
    | List         { items    :: LinkListTox Terms                             }
    | Marked       { marker   :: LinkTo Terms , body  :: LinkTo Terms          }
    | Marker       { id       :: Word64                                        }
    | SectionLeft  { operator :: LinkTo Terms , body  :: LinkTo Terms          }
    | SectionRight { operator :: LinkTo Terms , body  :: LinkTo Terms          }
    | Modify       { base     :: LinkTo Terms , path  :: Vector Name
                   , operator :: Name         , value :: LinkTo Terms          }
    | Seq          { former   :: LinkTo Terms , later :: LinkTo Terms          }
    | Tuple        { items    :: LinkListTox Terms                             }
    | Typed        { base     :: LinkTo Terms , tp    :: LinkTo Terms          }
 |]

