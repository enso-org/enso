{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Data.Class where

import Prologue

import qualified Data.Map         as Map
import qualified Luna.IR          as IR
import qualified Luna.Pass.Attr   as Attr

import Data.Graph.Data.Component.Class (unsafeNull)
import Luna.Pass.Sourcing.Data.Def



-------------------------
-- === Constructor === --
-------------------------

-- === Definition === --

newtype Constructor = Constructor { _arity :: Int }


-- === Instances === --

instance Show Constructor where
    show _ = "Constructor"



-------------------
-- === Class === --
-------------------

-- === Definition === --

data Class = Class
    { _constructors :: Map.Map IR.Name Constructor
    , _methods      :: DefsMap
    , _root         :: IR.Term IR.Record
    } deriving Show
makeLenses ''Constructor
makeLenses ''Class


-- === Instances === --

type instance Attr.Type Class = Attr.Atomic
instance Default Class where
    def = Class def def unsafeNull

