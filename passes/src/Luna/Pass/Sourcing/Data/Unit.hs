{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Data.Unit where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

import Luna.Pass.Sourcing.Data.Def
import Luna.Pass.Sourcing.Data.Class

data Unit = Unit
    { _definitions :: DefsMap
    , _classes     :: Map.Map IR.Name (Documented Class)
    } deriving Show
type instance Attr.Type Unit = Attr.Atomic
instance Default Unit where
    def = Unit def def

newtype Imports = Imports [IR.Qualified] deriving Show
type instance Attr.Type Imports = Attr.Atomic
instance Default Imports where
    def = Imports def

data UnitLocation = Graph (IR.Term IR.Unit) | Precompiled Unit deriving Show

data UnitRef = UnitRef
    { _root    :: UnitLocation
    , _imports :: Imports
    } deriving Show

newtype UnitRefsMap = UnitRefsMap (Map.Map IR.Qualified UnitRef)
type instance Attr.Type UnitRefsMap = Attr.Atomic
instance Default UnitRefsMap where
    def = UnitRefsMap def

newtype Name = Name IR.Qualified
type instance Attr.Type Name = Attr.Atomic
instance Default Name where
    def = Name ""

makeLenses ''UnitRef
makeLenses ''Unit
makeLenses ''UnitRefsMap
makeLenses ''Imports

