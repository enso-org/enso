{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.Data.Invalid where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr



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


