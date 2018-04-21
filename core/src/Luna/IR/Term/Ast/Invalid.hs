module Luna.IR.Term.Ast.Invalid where

import Prologue hiding (Symbol)

import qualified Foreign.Storable.Deriving as Storable


-----------------------------
-- === Invalid Symbols === --
-----------------------------

-- === Definition === --

data Symbol
    = ForeignImportSafety
    | FunctionHeader
    | FunctionBlock
    | UnexpectedSuffix {- len -} !Int
    | CaselessNameHead
    deriving (Eq, Ord, Generic, Show)

Storable.derive ''Symbol
instance NFData Symbol
