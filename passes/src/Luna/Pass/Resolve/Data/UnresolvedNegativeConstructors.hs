module Luna.Pass.Resolve.Data.UnresolvedNegativeConstructors where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype UnresolvedNegativeConstructors = UnresolvedNegativeConstructors [IR.Term IR.Cons]
type instance Attr.Type UnresolvedNegativeConstructors = Attr.Atomic
instance Default UnresolvedNegativeConstructors where
    def = UnresolvedNegativeConstructors def

makeLenses ''UnresolvedNegativeConstructors
