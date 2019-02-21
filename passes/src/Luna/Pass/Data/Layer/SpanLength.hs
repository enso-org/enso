{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.SpanLength where

import           Prologue

import qualified Data.Graph.Data.Layer.Class        as Layer
import qualified Data.Graph.Data.Layer.Layout       as Layout

import Data.Graph.Data.Layer.Class (Layer)
import Data.Text.Position          (Delta(..))



------------------------
-- === SpanLength === --
------------------------

data SpanLength deriving Generic

instance Layer SpanLength where
    type Cons SpanLength = Layer.Simple Delta
    type Layout SpanLength layout = Layout.Get SpanLength layout
    manager = Layer.staticManager

