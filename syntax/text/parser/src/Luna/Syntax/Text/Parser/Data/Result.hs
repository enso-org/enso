{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.Data.Result where

import Prologue

import qualified Luna.IR                as IR
import qualified Luna.Pass.Attr         as Attr
import qualified Data.Graph.Data.Component.Class as Component



--------------------
-- === Result === --
--------------------

newtype Result = Result (IR.Term IR.Unit) deriving (Show, Eq)
type instance Attr.Type Result = Attr.Atomic
makeLenses ''Result

instance Default Result where
    def = Result Component.unsafeNull ; {-# INLINE def #-}

