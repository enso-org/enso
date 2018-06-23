module Luna.Pass.Sourcing.Data.LoaderQueue where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype LoaderQueue = LoaderQueue [IR.Qualified]
type instance Attr.Type LoaderQueue = Attr.Atomic
instance Default LoaderQueue where
    def = LoaderQueue def

makeLenses ''LoaderQueue
