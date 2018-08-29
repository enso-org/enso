{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Data.AccQueue where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype AccQueue = AccQueue [IR.Term IR.Acc]
makeLenses ''AccQueue

type instance Attr.Type AccQueue = Attr.Atomic
instance Default AccQueue where
    def = wrap def

register :: Attr.Editor AccQueue m => IR.Term IR.Acc -> m ()
register = Attr.modify_ @AccQueue . over wrapped . (:)
{-# INLINE register #-}

registers :: Attr.Editor AccQueue m => [IR.Term IR.Acc] -> m ()
registers = Attr.modify_ @AccQueue . over wrapped . (<>)
{-# INLINE registers #-}

