{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.NodeMeta where

import           Prologue

import qualified Control.Monad.State.Layered        as State
import qualified Data.Construction                  as Data
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
import Foreign.Storable.Tuple                ()
import Foreign.Storable.Utils                ()


data Position = Position
    { _x :: Double
    , _y :: Double
    } deriving (Eq, Generic, Show)

type TextLike = UnmanagedSmallVector 0 Char

data NodeMetaLike = NodeMetaLike
    { _position           :: Position
    , _displayResult      :: Bool
    , _selectedVisualizer :: Maybe (TextLike, TextLike)
    } deriving (Eq, Generic, Show)

makeLenses ''Position
makeLenses ''NodeMetaLike

Storable.deriveNoContext ''Position
Storable.deriveNoContext ''NodeMetaLike
GTraversable.derive ''NodeMetaLike
GTraversable.derive ''Position

instance MonadIO m
      => FoldClass.Builder Buffer.CopyInitialization2 m NodeMetaLike where
    build = \(NodeMetaLike _ _ x) a
        -> FoldClass.build @Buffer.CopyInitialization2 x a
    {-# INLINE build #-}

instance (State.Monad Buffer.StoreDynState m, MonadIO m)
      => FoldClass.Builder Buffer.CopyInitialization m NodeMetaLike where
    build = \(NodeMetaLike _ _ x) a
        -> FoldClass.build @Buffer.CopyInitialization x a
    {-# INLINE build #-}

instance MonadIO m
      => FoldClass.Builder Buffer.Discovery m NodeMetaLike where
    build = \(NodeMetaLike _ _ x) a
        -> FoldClass.build @Buffer.Discovery x a
    {-# INLINE build #-}

instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep
                                          (Fold.Discovery
                                              '[IR.Terms, IR.Links])))
                        m (Maybe NodeMetaLike)

data Meta deriving Generic
instance Layer Meta where
    type Cons Meta = Layer.Simple (Maybe NodeMetaLike)
    type Layout Meta layout = Layout.Get Meta layout
    manager = Layer.dynamicManager

