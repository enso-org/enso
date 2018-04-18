{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Attributes where

import Prologue hiding (Span, String, Type, span, (<|))

import Type.Any (AnyType)

import qualified Luna.IR                as IR
import qualified Luna.Pass.Attr         as Attr
import qualified OCI.IR.Component.Class as Component


----------------------
-- === Invalids === --
----------------------

-- === Definition === --

newtype Invalids = Invalids [IR.SomeTerm]
    deriving (Show, Default, Mempty, Semigroup)
type instance Attr.Type Invalids = Attr.Atomic
makeLenses ''Invalids


-- -- === Utils === --

registerInvalid :: Attr.Editor Invalids m => IR.SomeTerm -> m ()
registerInvalid t = Attr.modify_ @Invalids $ wrapped %~ (t:) ; {-# INLINE registerInvalid #-}



--------------------
-- === Result === --
--------------------

newtype Result = Result (IR.Term IR.Unit) deriving (Show, Eq)
makeLenses ''Result

type instance Attr.Type Result = Attr.Atomic

instance Default Result where
    def = Result Component.unsafeNull ; {-# INLINE def #-}
