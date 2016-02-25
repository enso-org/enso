{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where

import           Data.Graph.Builder.Class               as X
import           Luna.Syntax.Model.Network.Builder.Term as X


import           Prologue                hiding (read, Getter, (#))
import           Control.Monad           (forM)
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph
import           Data.Container
import           Data.Layer.Cover
import           Data.Construction
import           Data.Index              (idx)
import           Data.Prop
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.IntSet             as IntSet

import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Term (Draft)
import           Luna.Evaluation.Runtime        (Static)


-------------------
-- === Utils === --
-------------------

importStructure :: ( node  ~ (NetLayers a :<: Draft Static)
                   , edge  ~ (Link node)
                   , graph ~ Hetero (VectorGraph n e c)
                   , BiCastable e edge
                   , BiCastable n node
                   , MonadBuilder graph m
                   , Referred Node n graph
                   , Constructor m (Ref Node node)
                   , Constructor m (Ref Edge edge)
                   ) => [(Ref Node node, node)] -> [(Ref Edge edge, edge)] -> m (Map (Ref Node node) (Ref Node node))
importStructure nodes edges = do
    let foreignNodeRefs = fst <$> nodes
        foreignEdgeRefs = fst <$> edges

    newNodeRefs <- mapM construct $ snd <$> nodes

    let nodeTrans = Map.fromList $ zip foreignNodeRefs newNodeRefs
        foreignEs  = snd <$> edges
        es         = foreignEs & over (mapped . source) unsafeTranslateNode
                               & over (mapped . target) unsafeTranslateNode
                   where
                   unsafeTranslateNode i = fromJust $ Map.lookup i nodeTrans

    newEdgeRefs <- forM es construct
    let edgeTrans = Map.fromList $ zip foreignEdgeRefs newEdgeRefs

    forM newNodeRefs $ \ref -> do
        node <- read ref
        let nodeWithFixedEdges = node & over covered (fmapInputs unsafeTranslateEdge)
                                      & over (prop Succs . mapped) unsafeTranslateEdge
                                      & over (prop Type)           unsafeTranslateEdge
                where
                unsafeTranslateEdge i = fromJust $ Map.lookup i edgeTrans
        write ref nodeWithFixedEdges

    return nodeTrans

merge :: ( node  ~ (NetLayers a :<: Draft Static)
         , edge  ~ (Link node)
         , graph ~ Hetero (VectorGraph n e c)
         , BiCastable e edge
         , BiCastable n node
         , MonadBuilder graph m
         , Referred Node n graph
         , Constructor m (Ref Node node)
         , Constructor m (Ref Edge edge)
         ) => graph -> m (Map (Ref Node node) (Ref Node node))
merge g = do
    let foreignNodeRefs = Ref <$> usedIxes (g ^. wrapped . nodeGraph)
        foreignEdgeRefs = Ref <$> usedIxes (g ^. wrapped . edgeGraph)
        foreignNodes    = flip view g . focus <$> foreignNodeRefs
        foreignEdges    = flip view g . focus <$> foreignEdgeRefs
    importStructure (zip foreignNodeRefs foreignNodes) (zip foreignEdgeRefs foreignEdges)

dupCluster :: forall graph node edge clus n e c a m .
              ( node  ~ NetNode a
              , edge  ~ (Link node)
              , clus  ~ NetCluster a
              , graph ~ Hetero (VectorGraph n e c)
              , BiCastable e edge
              , BiCastable n node
              , BiCastable c clus
              , MonadBuilder graph m
              , Referred Node n graph
              , Constructor m (Ref Node    node)
              , Constructor m (Ref Edge    edge)
              , Constructor m (Ref Cluster clus)
              ) => Ref Cluster clus -> String -> m (Ref Cluster clus, Map (Ref Node node) (Ref Node node))
dupCluster cluster name = do
    nodeRefs <- members cluster
    (nodes :: [node]) <- mapM read nodeRefs
    let gatherEdges n = foldr IntSet.insert mempty (view idx <$> ((n # Inputs) ++ [n ^. prop Type]))
    let edgeRefs = Ref <$> (IntSet.toList $ foldr IntSet.union mempty (gatherEdges <$> nodes))
    edges <- mapM read edgeRefs
    trans <- importStructure (zip nodeRefs nodes) (zip edgeRefs edges)
    cl <- subgraph
    withRef cl $ prop Name .~ name
    mapM (flip include cl) $ Map.elems trans
    return (cl, trans)
