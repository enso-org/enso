{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Luna.IR.Term.Ast.Class where

import Prologue hiding (Imp, imp, seq)

import qualified Data.Generics.Traversable.Deriving as GTraversable
-- import qualified Data.Graph.Store.External          as ExternalStorable
import qualified Foreign.Storable.Deriving as Storable
import qualified Luna.IR.Term.Ast.Invalid  as Invalid
import qualified Luna.IR.Term.Format       as Format
import qualified OCI.IR.Term.Definition    as Term

-- import Data.Graph.Store.External (ExternalFieldStorable, ExternalStorable)
import Data.Vector.Storable.Foreign (Vector)
import OCI.Data.Name                (Name)
import OCI.IR.Term.Class            (Terms)
import OCI.IR.Term.Definition       (LinkTo, LinksTo)

-- FIXME: remove when refactoring Cmp instances
import Luna.IR.Term.Core ()



---------------------
-- === Imports === --
---------------------

-- === Helpers === --

data ImportSourceData
    = Relative (Vector Name)
    | Absolute (Vector Name)
    | World
    deriving (Eq, Generic, Show)
Storable.derive     ''ImportSourceData
GTraversable.derive ''ImportSourceData
-- instance ExternalStorable ImportSourceData
-- instance ExternalFieldStorable ImportSourceData
-- instance ExternalStorable.SizeBuilder ImportSourceData

data ImportTargetData
    = Everything
    | Listed (Vector Name)
    deriving (Eq, Generic, Show)
Storable.derive     ''ImportTargetData
GTraversable.derive ''ImportTargetData
-- instance ExternalStorable ImportTargetData
-- instance ExternalFieldStorable ImportTargetData
-- instance ExternalStorable.SizeBuilder ImportTargetData


-- === FFI === --

-- FIXME: May be able to become a `Maybe` pending discussion in the
-- following issue: https://github.com/luna/luna/issues/179
data ForeignImportType
    = Default
    | Safe
    | Unsafe
    deriving (Eq, Generic, Show)
Storable.derive     ''ForeignImportType
GTraversable.derive ''ForeignImportType
-- instance ExternalStorable ForeignImportType
-- instance ExternalFieldStorable ForeignImportType
-- instance ExternalStorable.SizeBuilder ForeignImportType


-----------------
-- === Ast === --
-----------------

-- === Definition === --

Term.define [d|
 data Ast
    = AccSection   { path     :: Vector Name                                   }
    | Disabled     { body     :: LinkTo Terms                                  }
    | Documented   { doc      :: Vector Char  , base   :: LinkTo  Terms        }
    | Function     { name     :: LinkTo Terms , args   :: LinksTo Terms
                   , body     :: LinkTo Terms                                  }
    | Grouped      { body     :: LinkTo Terms                                  }
    | Imp          { source   :: LinkTo Terms , target :: ImportTargetData     }
    | ImportHub    { imps     :: LinksTo Terms                                 }
    | ImportSource { body     :: ImportSourceData                              }
    | Invalid      { desc     :: Invalid.Symbol                                }
    | List         { items    :: LinksTo Terms                                 }
    | Marked       { marker   :: LinkTo Terms , body   :: LinkTo Terms         }
    | Marker       { id       :: Word64                                        }
    | SectionLeft  { operator :: LinkTo Terms , body   :: LinkTo Terms         }
    | SectionRight { operator :: LinkTo Terms , body   :: LinkTo Terms         }
    | Modify       { base     :: LinkTo Terms , path   :: Vector Name
                   , operator :: Name         , value  :: LinkTo Terms         }
    | Metadata     { content  :: Vector Char                                   }
    | Record       { isNative :: Bool         , name   :: Name
                   , params   :: LinksTo Terms, conss  :: LinksTo Terms
                   , decls    :: LinksTo Terms                                 }
    | RecordCons   { name     :: Name         , fields :: LinksTo Terms        }
    | RecordFields { names    :: Vector Name  , tp     :: LinkTo Terms         }
    | Seq          { former   :: LinkTo Terms , later  :: LinkTo Terms         }
    | Tuple        { items    :: LinksTo Terms                                 }
    | Typed        { base     :: LinkTo Terms , tp     :: LinkTo Terms         }
    | Unit         { imps     :: LinkTo Terms , units  :: LinksTo Terms
                   , cls      :: LinkTo Terms                                  }
    -- DEPRECATED:
    | FunctionSig  { name     :: LinkTo Terms , sig    :: LinkTo Terms         }
 |]


-- === FFI === --

Term.define [d|
 data Ast
    = ForeignImport       { lang    :: Name         , lst  :: LinksTo Terms }
    | ForeignImportList   { loc     :: LinkTo Terms , imps :: LinksTo Terms }
    | ForeignImportSymbol { safety  :: LinkTo Terms , name :: LinkTo  Terms
                          , locName :: Name         , tp   :: LinkTo  Terms }
    | ForeignImportSafety { safety  :: ForeignImportType                    }
 |]
