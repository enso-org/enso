{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Data.Root where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

import Data.Graph.Data.Component.Class (unsafeNull)



------------------
-- === Root === --
------------------

-- === Definition === --

newtype Root = Root IR.SomeTerm
makeLenses ''Root


-- === Instances === --

type instance Attr.Type Root = Attr.Atomic
instance Default Root where
    def = Root unsafeNull

