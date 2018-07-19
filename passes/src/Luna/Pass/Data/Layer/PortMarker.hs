{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.PortMarker where

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
import Data.UUID.Types (UUID)


data OutPortRefLike = OutPortRefLike { _srcNodeLoc :: UUID
                                     , _srcPortId  :: Foreign.Vector Int
                                     } deriving (Eq, Generic)

makeLenses ''OutPortRefLike

instance Show OutPortRefLike where
    show _ = "OutPortRefLike"

Storable.deriveNoContext ''OutPortRefLike
GTraversable.derive ''OutPortRefLike
GTraversable.derive ''UUID

data PortMarker deriving Generic
instance Layer PortMarker where
    type Cons PortMarker = Layer.Simple (Maybe OutPortRefLike)
    type Layout PortMarker layout = Layout.Get PortMarker layout
    manager = Layer.staticManager


instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m (Maybe OutPortRefLike)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m (Maybe OutPortRefLike)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m (Maybe OutPortRefLike)
instance Monad m => FoldClass.Builder Buffer.Discovery  m (Maybe OutPortRefLike)
