module Luna.Syntax.Text.Parser.State.Result where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Luna.IR                         as IR
import qualified Luna.Pass.Attr                  as Attr



--------------------
-- === Result === --
--------------------

-- === Definition === --

newtype Result = Result IR.SomeTerm deriving (Show, Eq)
type instance Attr.Type Result = Attr.Atomic
makeLenses ''Result


-- === Instances === --

instance Default Result where
    def = Result Component.unsafeNull
    {-# INLINE def #-}
