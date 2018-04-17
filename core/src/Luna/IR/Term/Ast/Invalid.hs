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
    | VarName VarName
    deriving (Eq, Ord, Generic, Show)

data VarName
    = CaselessHeader
    | UnderscoresOnly
    | UnexpectedSuffix {- off -} !Int
    deriving (Eq, Ord, Generic, Show)

Storable.derive ''Symbol
Storable.derive ''VarName
instance NFData Symbol
instance NFData VarName


-- === Smart constructors === --

caselessHeader :: Symbol
caselessHeader = VarName CaselessHeader ; {-# INLINE caselessHeader #-}

underscoresOnly :: Symbol
underscoresOnly = VarName UnderscoresOnly ; {-# INLINE underscoresOnly #-}

unexpectedSuffix :: Int -> Symbol
unexpectedSuffix = VarName . UnexpectedSuffix ; {-# INLINE unexpectedSuffix #-}
