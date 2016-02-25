module Luna.Syntax.Name.Ident.Class where

import Prelude.Luna

import Data.Char                   (isLower, isUpper)
import Luna.Syntax.Name.Class
import Luna.Syntax.Name.Ident.Type


-------------------
-- === Ident === --
-------------------

-- === Definition === --

newtype Ident t = Ident NameBase deriving (Show, Eq, Ord)
makeWrapped ''Ident

type VarIdent  = Ident Var
type TypeIdent = Ident Type


-- === Utils === --

varIdent :: String -> Ident Var
varIdent = wrap' ∘ fromString
{-# INLINE varIdent #-}

typeIdent :: String -> Ident Type
typeIdent = wrap' ∘ fromString
{-# INLINE typeIdent #-}


-- === Instances === --

type instance Name (Ident t) = Ident t
instance   HasName (Ident t) where name = id ; {-# INLINE name #-}

instance IsString (Ident Var ) where fromString = varIdent           ; {-# INLINE fromString #-}
instance IsString (Ident Type) where fromString = typeIdent          ; {-# INLINE fromString #-}
instance ToString (Ident t)    where toString   = toString ∘ unwrap' ; {-# INLINE toString #-}

instance Repr s (Ident t) where repr = repr ∘ unwrap' ; {-# INLINE repr #-}
