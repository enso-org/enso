{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.SpanOffset where

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


data SpanOffset deriving Generic
instance Layer SpanOffset where
    type Cons SpanOffset = Layer.Simple Delta
    type Layout SpanOffset layout = Layout.Get SpanOffset layout
    manager = Layer.staticManager

instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m Delta
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m Delta
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m Delta
instance Monad m => FoldClass.Builder Buffer.Discovery  m Delta

