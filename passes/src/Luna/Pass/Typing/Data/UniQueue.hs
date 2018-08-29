{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Data.UniQueue where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype UniQueue = UniQueue [IR.Term IR.Unify]
makeLenses ''UniQueue

type instance Attr.Type UniQueue = Attr.Atomic
instance Default UniQueue where
    def = wrap def

register :: Attr.Editor UniQueue m => IR.Term IR.Unify -> m ()
register = Attr.modify_ @UniQueue . over wrapped . (:)
{-# INLINE register #-}

registers :: Attr.Editor UniQueue m => [IR.Term IR.Unify] -> m ()
registers = Attr.modify_ @UniQueue . over wrapped . (<>)
{-# INLINE registers #-}

