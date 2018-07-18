{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.NodeMeta where

import           Prologue

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
import qualified Data.Vector.Storable.Foreign       as Foreign
import qualified Foreign.Storable.Deriving          as Storable

import Data.Graph.Data.Layer.Class (Layer)
import Foreign.Storable.Tuple      ()
import Foreign.Storable.Utils      ()


data Position = Position { _x :: Double
                         , _y :: Double
                         } deriving (Eq, Generic)

type TextLike = Foreign.Vector Char

data NodeMetaLike = NodeMetaLike { _position           :: Position
                                 , _displayResult      :: Bool
                                 , _selectedVisualizer :: Maybe (TextLike, TextLike)
                                 } deriving (Eq, Generic)

instance Show NodeMetaLike where
	show _ = "NodeMetaLike"

makeLenses ''Position
makeLenses ''NodeMetaLike

Storable.deriveNoContext ''Position
Storable.deriveNoContext ''NodeMetaLike
GTraversable.derive ''NodeMetaLike
GTraversable.derive ''Position

data Meta deriving Generic
instance Layer Meta where
    type Cons Meta = Layer.Simple (Maybe NodeMetaLike)
    type Layout Meta layout = Layout.Get Meta layout
    manager = Layer.dynamicManager


instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m (Maybe NodeMetaLike)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m (Maybe NodeMetaLike)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m (Maybe NodeMetaLike)
instance Monad m => FoldClass.Builder Buffer.Discovery  m (Maybe NodeMetaLike)
