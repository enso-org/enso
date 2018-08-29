{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.SpanLength where

import           Prologue

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.Graph.Data.Layer.Class        as Layer
import qualified Data.Graph.Data.Layer.Layout       as Layout
import qualified Data.Graph.Fold.Class              as FoldClass
import qualified Data.Graph.Fold.Scoped             as Fold
import qualified Data.Graph.Fold.Deep               as Fold
import qualified Data.Graph.Fold.Partition          as Fold
import qualified Data.Graph.Store.Buffer            as Buffer
import qualified Data.Graph.Store.Size.Discovery    as Buffer
import qualified Data.Vector.Storable.Foreign       as Foreign
import qualified Foreign.Storable.Deriving          as Storable

import Data.Graph.Data.Layer.Class (Layer)
import Data.Text.Position          (Delta(..))


data SpanLength deriving Generic
instance Layer SpanLength where
    type Cons SpanLength = Layer.Simple Delta
    type Layout SpanLength layout = Layout.Get SpanLength layout
    manager = Layer.staticManager

