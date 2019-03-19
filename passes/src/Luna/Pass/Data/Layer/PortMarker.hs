{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.PortMarker where

import           Prologue

import qualified Control.Monad.State.Layered        as State
import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.Graph.Data.Layer.Class        as Layer
import qualified Data.Graph.Data.Layer.Layout       as Layout
import qualified Data.Graph.Fold.Class              as FoldClass
import qualified Data.Graph.Fold.Scoped             as Fold
import qualified Data.Graph.Fold.Deep               as Fold
import qualified Data.Graph.Fold.Partition          as Fold
import qualified Data.Graph.Store.Buffer            as Buffer
import qualified Data.Graph.Store.Size.Discovery    as Buffer
import qualified Foreign.Storable.Deriving          as Storable
import qualified Luna.IR                            as IR

import Data.Graph.Data.Layer.Class           (Layer)
import Data.Mutable.Storable.SmallAutoVector (UnmanagedSmallVector)
import Data.UUID.Types                       (UUID)


data OutPortRefLike = OutPortRefLike
    { _srcNodeLoc :: UUID
    , _srcPortId  :: UnmanagedSmallVector 0 Int
    } deriving (Eq, Generic, Show)

makeLenses ''OutPortRefLike

Storable.deriveNoContext ''OutPortRefLike
GTraversable.derive ''OutPortRefLike
GTraversable.derive ''UUID

data PortMarker deriving Generic
instance Layer PortMarker where
    type Cons PortMarker = Layer.Simple (Maybe OutPortRefLike)
    type Layout PortMarker layout = Layout.Get PortMarker layout
    manager = Layer.dynamicManager


instance MonadIO m
      => FoldClass.Builder Buffer.CopyInitialization2 m OutPortRefLike where
    build = \(OutPortRefLike _ x) a
        -> FoldClass.build @Buffer.CopyInitialization2 x a
    {-# INLINE build #-}

instance (State.Monad Buffer.StoreDynState m, MonadIO m)
      => FoldClass.Builder Buffer.CopyInitialization m OutPortRefLike where
    build = \(OutPortRefLike _ x) a
        -> FoldClass.build @Buffer.CopyInitialization x a
    {-# INLINE build #-}

instance MonadIO m
      => FoldClass.Builder Buffer.Discovery m OutPortRefLike where
    build = \(OutPortRefLike _ x) a
        -> FoldClass.build @Buffer.Discovery x a
    {-# INLINE build #-}

instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep
                                          (Fold.Discovery
                                              '[IR.Terms, IR.Links])))
                        m (Maybe OutPortRefLike)

