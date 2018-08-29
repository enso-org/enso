{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Data.AppQueue where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype AppQueue = AppQueue [IR.Term IR.App]
makeLenses ''AppQueue

type instance Attr.Type AppQueue = Attr.Atomic
instance Default AppQueue where
    def = wrap def

register :: Attr.Editor AppQueue m => IR.Term IR.App -> m ()
register = Attr.modify_ @AppQueue . over wrapped . (:)
{-# INLINE register #-}

registers :: Attr.Editor AppQueue m => [IR.Term IR.App] -> m ()
registers = Attr.modify_ @AppQueue . over wrapped . (<>)
{-# INLINE registers #-}

