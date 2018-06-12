module Data.Graph.Transform.Substitute where

import Prologue

import qualified Data.Graph.Data.Layer.Class            as Layer
import qualified Data.Graph.Data.Layer.Layout           as Layout
import qualified Data.Set.Mutable.Class                 as Set
import qualified Data.Graph.Component.Edge.Construction as Construction
import qualified Data.Graph.Component.Edge.Destruction  as DestructEdge
import qualified Data.Graph.Component.Node.Destruction  as DestructNode

import Control.Monad                   (filterM)
import Data.Graph.Component.Edge.Class (type (*-*), Edge, Edges, Source,
                                        Target, isCyclic)
import Data.Graph.Component.Node.Class (Node, Nodes)
import Data.Graph.Component.Node.Layer (Users)
import Data.Graph.Data                 (Component)


type Replace m = ( Layer.Reader (Component Edges) Source m
                 , Layer.Writer (Component Edges) Source m
                 , Layer.Reader (Component Nodes) Users m
                 , MonadIO m
                 )

replaceSource :: Replace m => Node a -> Edge (a *-* b) -> m ()
replaceSource = \newSrc edge -> do
    oldSrcUsers <- Layer.read @Users =<< Layer.read @Source edge
    Set.delete oldSrcUsers $ Layout.unsafeRelayout edge
    newSrcUsers <- Layer.read @Users newSrc
    Set.insert newSrcUsers $ Layout.unsafeRelayout edge
    Layer.write @Source edge newSrc
{-# INLINE replaceSource #-}

substitute ::
    ( Replace m
    , Layer.Reader (Component Edges) Target m
    ) => Node a -> Node b -> m ()
substitute = \new old -> do
     succs <- Set.toList =<< Layer.read @Users old
     withoutCycles <- filterM (\a -> not <$> isCyclic a) $
        map Layout.unsafeRelayout succs
     mapM_ (replaceSource new) withoutCycles
{-# INLINE substitute #-}

replace ::
    ( DestructNode.DeleteSubtree m
    , Replace m
    ) => Node a -> Node b -> m ()
replace = \new old -> substitute new old >> DestructNode.deleteSubtree old
{-# INLINE replace #-}

type Reconnect l m = ( DestructEdge.Delete m
                     , Construction.Creator m
                     , Layer.Editor (Component Nodes) l m
                     )

reconnectLayer :: forall l m a b.
    ( Reconnect l m
    , Layer.Data l b ~ Edge (a *-* b)
    ) => Node a -> Node b -> m ()
reconnectLayer = \src tgt -> do
    old <- Layer.read @l tgt
    DestructEdge.delete old
    link <- Construction.new src tgt
    Layer.write @l tgt link
{-# INLINE reconnectLayer #-}
