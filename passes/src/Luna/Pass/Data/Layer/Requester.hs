{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.Requester where

import Prologue

import qualified Data.Construction                     as Data
import qualified Data.Generics.Traversable.Deriving    as GTraversable
import qualified Data.Graph.Fold.Class                 as FoldClass
import qualified Data.Graph.Fold.Scoped                as Fold
import qualified Data.Graph.Fold.Deep                  as Fold
import qualified Data.Graph.Fold.Partition             as Fold
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Component.Edge             as Edge
import qualified Data.Graph.Component.Edge.Destruction as Edge
import qualified Data.Graph.Store.Buffer               as Buffer
import qualified Data.Graph.Store.Size.Discovery       as Buffer
import qualified Data.Vector.Storable.Foreign          as Foreign
import qualified Foreign.Storable.Deriving             as Storable
import qualified Luna.IR                               as IR
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass.Data.Error                  as Error
import qualified Luna.Pass.Typing.Data.Target          as Target

import Data.Graph.Data.Component.Maybe       (MaybeComponent (..))
import Data.Graph.Data.Layer.Class           (Layer)
import Data.Graph.Component.Edge.Class       (type (*-*))
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Component.Edge.Class       (Edges)
import Luna.Pass.Typing.Data.Target          (Target)

data Requester deriving Generic
instance Layer Requester where
    type Cons   Requester        = MaybeComponent Edges
    type Layout Requester layout = () *-* layout
    manager = Layer.staticManager

newtype ArisingFromData = ArisingFromData (Maybe (Foreign.Vector Target))
    deriving (Default, Data.ShallowDestructor m)
makeLenses ''ArisingFromData
Storable.deriveNoContext ''ArisingFromData
GTraversable.derive ''ArisingFromData

instance Show ArisingFromData where
    show _ = "ArisingFromData"

data ArisingFrom deriving Generic
instance Layer ArisingFrom where
    type Cons ArisingFrom = Layer.Simple ArisingFromData
    manager = Layer.staticManager

-- FIXME[MK]: These are wrong. They leak memory and cause segfaults when trying to free the layer.
instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m ArisingFromData
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m ArisingFromData
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m ArisingFromData
instance Monad m => FoldClass.Builder Buffer.Discovery           m ArisingFromData

getRequesterEdge :: Layer.Reader IR.Term Requester m
                 => IR.Term a -> m (Maybe IR.SomeLink)
getRequesterEdge = \t -> do
    link <- unwrap <$> Layer.read @Requester (Layout.relayout t :: IR.SomeTerm)
    return $ Layout.relayout <$> link
{-# INLINE getRequesterEdge #-}

getRequester ::
    ( Layer.Reader IR.Term Requester m
    , Layer.Reader IR.Link IR.Source m
    ) => IR.Term a -> m (Maybe IR.SomeTerm)
getRequester = traverse IR.source <=< getRequesterEdge
{-# INLINE getRequester #-}

setRequester ::
    ( Layer.Editor IR.Term Requester m
    , Edge.Creator m
    , Edge.Delete  m
    ) => Maybe (IR.Term a) -> IR.Term b -> m ()
setRequester = \src tgt -> do
    old <- getRequesterEdge tgt
    traverse_ Edge.delete old
    l <- traverse (flip Edge.new tgt) src
    Layer.write @Requester tgt (wrap $ Layout.relayout <$> l)
{-# INLINE setRequester #-}

setArising ::
    ( Layer.Editor IR.Term ArisingFrom m
    ) => [Target] -> IR.Term b -> m ()
setArising = \err tgt -> do
    old <- Layer.read @ArisingFrom tgt
    -- FIXME[MK]: Uncomment when we can safely delete the layer without segfaults.
    {-Data.destructShallow old-}
    new <- Foreign.fromList err
    Layer.write @ArisingFrom tgt $ wrap $ Just new
{-# INLINE setArising #-}

getArising ::
    ( Layer.Reader IR.Term ArisingFrom m
    ) => IR.Term a -> m [Target]
getArising = \r -> do
    d <- unwrap <$> Layer.read @ArisingFrom r
    maybe (pure mempty) Foreign.toList d
{-# INLINE getArising #-}

pushArising ::
    ( Layer.Editor IR.Term ArisingFrom m
    ) => Target -> IR.Term a -> m ()
pushArising = \item r -> do
    old <- getArising r
    setArising (item:old) r
