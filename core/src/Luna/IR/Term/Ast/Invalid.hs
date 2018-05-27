{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Ast.Invalid where

import Prologue hiding (Symbol)

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Foreign.Storable.Deriving          as Storable

import Foreign.PartitionStorable (ExternalStorable)


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
    | Literal InvalidLiteral
    deriving (Eq, Ord, Generic, Show)
instance ExternalStorable Symbol

data InvalidLiteral
    = String InvalidString
    deriving (Eq, Ord, Generic, Show)

data InvalidString
    = EscapeCode
    deriving (Eq, Ord, Generic, Show)

Storable.derive     ''Symbol
Storable.derive     ''InvalidLiteral
Storable.derive     ''InvalidString
GTraversable.derive ''Symbol
GTraversable.derive ''InvalidLiteral
GTraversable.derive ''InvalidString
instance NFData Symbol
instance NFData InvalidLiteral
instance NFData InvalidString
