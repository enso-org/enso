{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Data.Class where

import Prologue

import qualified Data.Map         as Map
import qualified Luna.IR          as IR
import qualified Luna.Pass.Attr   as Attr
import qualified Data.Graph.Store as Store

import Data.Graph.Data.Component.Class (unsafeNull)
import Luna.Pass.Sourcing.Data.Def



newtype Constructor = Constructor { _arity :: Int }
instance Show Constructor where
    show _ = "Constructor"

data Class = Class
    { _constructors :: Map.Map IR.Name Constructor
    , _methods      :: DefsMap
    , _root         :: IR.Term IR.Record
    } deriving Show
type instance Attr.Type Class = Attr.Atomic
instance Default Class where
    def = Class def def unsafeNull

makeLenses ''Constructor
makeLenses ''Class

