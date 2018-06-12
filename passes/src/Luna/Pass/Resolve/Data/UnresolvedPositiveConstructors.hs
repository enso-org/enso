module Luna.Pass.Resolve.Data.UnresolvedPositiveConstructors where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype UnresolvedPositiveConstructors = UnresolvedPositiveConstructors [IR.Term IR.Cons]
type instance Attr.Type UnresolvedPositiveConstructors = Attr.Atomic
instance Default UnresolvedPositiveConstructors where
    def = UnresolvedPositiveConstructors def

makeLenses ''UnresolvedPositiveConstructors
