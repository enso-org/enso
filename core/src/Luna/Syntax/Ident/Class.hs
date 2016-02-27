module Luna.Syntax.Ident.Class where

import Prelude.Luna

import Data.Char                   (isLower, isUpper)
import Luna.Data.Name
import Luna.Syntax.Ident.Type


-------------------
-- === Ident === --
-------------------

-- === Definition === --

newtype Ident t = Ident Name deriving (Show, Eq, Ord)
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

instance IsString (Ident Var ) where fromString = varIdent           ; {-# INLINE fromString #-}
instance IsString (Ident Type) where fromString = typeIdent          ; {-# INLINE fromString #-}
instance ToString (Ident t)    where toString   = toString ∘ unwrap' ; {-# INLINE toString   #-}
instance HasName  (Ident t)    where name       = wrapped'           ; {-# INLINE name       #-}
instance Repr s   (Ident t)    where repr       = repr ∘ unwrap'     ; {-# INLINE repr       #-}
