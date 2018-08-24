{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Data.Def where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR
import qualified Luna.Runtime   as Luna

import Control.Lens.TH  (makePrisms)
import Data.Graph.Store (Rooted)

data Documented a = Documented
    { _documentation :: Maybe Text
    , _documented    :: a
    } deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Documented

data PrecompiledDef = PrecompiledDef
    { _value  :: Luna.Units -> Luna.Value
    , _header :: Rooted (IR.Term IR.DefHeader)
    }
makeLenses ''PrecompiledDef

instance Show PrecompiledDef where
    show _ = "Precompiled"

data Def = Body (IR.Term IR.Function) | Precompiled PrecompiledDef deriving (Show)
makePrisms ''Def

newtype DefsMap = DefsMap (Map.Map IR.Name (Documented Def)) deriving (Show, Default)
makeLenses ''DefsMap

