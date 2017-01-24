{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Redirect where

import Luna.Prelude
import Luna.IR.Layer.Class
import Luna.IR.Internal.IR


data Redirect
type instance LayerData Redirect t = Maybe SomeExpr
