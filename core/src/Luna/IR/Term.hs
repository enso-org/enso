{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term (module X) where

import Luna.IR.Term.Function as X
import Luna.IR.Term.Core     as X
import Luna.IR.Term.Cls      as X
import Luna.IR.Term.Unit     as X hiding (source, target, World, cls)
import Luna.IR.Term.World    as X

import Type.Container (Every)
import OCI.IR.Term
import Type.List

type instance Every TermType = CoreTerms <> '[Cls, Rec, World, Unit, UnitProxy, UnresolvedImport, UnresolvedImportSrc, UnresolvedImportHub, Import, ImportHub, ForeignImportList, ForeignLocationImportList, ForeignSymbolImport, ForeignImportSafety, ASGFunction, FunctionSig, RootedFunction, ASGRootedFunction]
