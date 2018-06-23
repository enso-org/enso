module Luna.Pass.Sourcing.Data.Class where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

import Luna.Pass.Sourcing.Data.Def

newtype Constructor = Constructor { _arity :: Int } deriving Show

data Class = Class
    { _constructors :: Map.Map IR.Name Constructor
    , _methods      :: DefsMap
    } deriving Show
type instance Attr.Type Class = Attr.Atomic
instance Default Class where
    def = Class def def

makeLenses ''Constructor
makeLenses ''Class
