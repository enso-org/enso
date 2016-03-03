-- FIXME[WD->MK]: there should be ban for using "Utils" name. Always :P
module Luna.Syntax.Model.Network.Builder.Utils.Merging where

import           Luna.Syntax.Model.Network.Builder.Term
import           Luna.Syntax.Model.Network.Builder.Layer
import           Prelude.Luna
import           Control.Monad                 (forM)
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph
import           Data.Container                (usedIxes)
import           Data.Layer.Cover
import           Data.Construction
import           Data.Index                    (idx)
import           Data.Prop
import           Data.Map                      (Map)
import           Data.Maybe                    (fromMaybe)
import           Data.List                     (partition)
import qualified Data.Map                      as Map
import qualified Data.IntSet                   as IntSet
import           Luna.Syntax.AST.Function      (Signature)

import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Term (Draft)
import           Luna.Evaluation.Runtime        (Static)
import qualified Luna.Syntax.AST.Function       as Function


-------------------
-- === Utils === --
-------------------

-- FIXME[MK]: Do not explicitly type stuff here as NetGraph, solve the problems with typing it differently

type NodeTranslator n = Ref Node n -> Ref Node n

mkNodeTranslator :: Map (Ref Node n) (Ref Node n) -> NodeTranslator n
mkNodeTranslator m r = case Map.lookup r m of
    Just res -> res
    Nothing  -> r

translateSignature :: NodeTranslator a -> Signature (Ref Node a) -> Signature (Ref Node a)
translateSignature f sig = sig & Function.self . mapped          %~ f
                               & Function.args . mapped . mapped %~ f
                               & Function.out                    %~ f

importStructure :: ( node  ~ (NetLayers :<: Draft Static)
                   , edge  ~ (Link node)
                   , graph ~ Hetero (VectorGraph n e c)
                   , BiCastable e edge
                   , BiCastable n node
                   , MonadBuilder graph m
                   , Referred Node n graph
                   , Constructor m (Ref Node node)
                   , Constructor m (Ref Edge edge)
                   , Connectible (Ref Node node) (Ref Node node) m
                   ) => [(Ref Node node, node)] -> [(Ref Edge edge, edge)] -> m (NodeTranslator node)
importStructure nodes' edges' = do
    let nodes           = filter ((/= universe) . fst) nodes'
        edges           = filter ((/= universe) . view target . snd) edges'
        foreignNodeRefs = fst <$> nodes
        foreignEdgeRefs = fst <$> edges

    newNodeRefs <- mapM (construct . (prop Succs .~ [])) $ snd <$> nodes

    let nodeTrans         = Map.fromList $ zip foreignNodeRefs newNodeRefs
        translateNode     = mkNodeTranslator nodeTrans
        translateEdgeEnds = (source %~ translateNode) . (target %~ translateNode)
        foreignEs         = snd <$> edges
        es                = translateEdgeEnds <$> foreignEs

    newEdgeRefs <- forM es $ \e -> connection (e ^. source) (e ^. target)
    let edgeTrans = Map.fromList $ zip foreignEdgeRefs newEdgeRefs

    forM newNodeRefs $ \ref -> do
        node <- read ref
        let nodeWithFixedEdges = node & over covered     (fmapInputs unsafeTranslateEdge)
                                      & over (prop Type) unsafeTranslateEdge
                where
                unsafeTranslateEdge i = fromMaybe i $ Map.lookup i edgeTrans
        write ref nodeWithFixedEdges

    return translateNode

importToCluster :: ( node  ~ (NetLayers :<: Draft Static)
         , edge  ~ (Link node)
         , graph ~ Hetero (VectorGraph n e c)
         , clus  ~ NetCluster
         , BiCastable e edge
         , BiCastable n node
         , BiCastable c clus
         , MonadBuilder graph m
         , Referred Node n graph
         , Constructor m (Ref Node node)
         , Constructor m (Ref Edge edge)
         , Connectible (Ref Node node) (Ref Node node) m
         ) => graph -> m (Ref Cluster clus, NodeTranslator node)
importToCluster g = do
    let foreignNodeRefs = Ref <$> usedIxes (g ^. wrapped . nodeGraph)
        foreignEdgeRefs = Ref <$> usedIxes (g ^. wrapped . edgeGraph)
        foreignNodes    = flip view g . focus <$> foreignNodeRefs
        foreignEdges    = flip view g . focus <$> foreignEdgeRefs
    trans <- importStructure (zip foreignNodeRefs foreignNodes) (zip foreignEdgeRefs foreignEdges)
    cls <- subgraph
    mapM (flip include cls) $ filter (/= universe) $ trans <$> foreignNodeRefs
    return (cls, trans)

dupCluster :: forall graph node edge clus n e c m .
              ( node  ~ NetNode
              , edge  ~ (Link node)
              , clus  ~ NetCluster
              , graph ~ Hetero (VectorGraph n e c)
              , BiCastable e edge
              , BiCastable n node
              , BiCastable c clus
              , MonadBuilder graph m
              , Referred Node n graph
              , Constructor m (Ref Node    node)
              , Constructor m (Ref Edge    edge)
              , Constructor m (Ref Cluster clus)
              , Connectible (Ref Node node) (Ref Node node) m
              ) => Ref Cluster clus -> String -> m (Ref Cluster clus, NodeTranslator node)
dupCluster cluster name = do
    nodeRefs <- members cluster
    (nodes :: [node]) <- mapM read nodeRefs
    let gatherEdges n = foldr IntSet.insert mempty (view idx <$> ((n # Inputs) ++ [n ^. prop Type]))
    let edgeRefs = Ref <$> (IntSet.toList $ foldr IntSet.union mempty (gatherEdges <$> nodes))
    edges <- mapM read edgeRefs
    trans <- importStructure (zip nodeRefs nodes) (zip edgeRefs edges)
    fptr  <- follow (prop Lambda) cluster
    cl <- subgraph
    withRef cl $ (prop Name   .~ name)
               . (prop Lambda .~ (translateSignature trans <$> fptr))
    mapM (flip include cl) $ trans <$> nodeRefs
    return (cl, trans)

universe :: Ref Node n
universe = Ref 0
