{-# LANGUAGE CPP #-}

module Luna.Syntax.Model.Network.Builder.Structural.Merging where

import           Luna.Syntax.Model.Network.Builder.Term
import           Luna.Syntax.Model.Network.Builder.Layer
import           Prelude.Luna
import           Control.Monad                 (forM)
import           Data.Graph
import           Data.Graph.Builder
import qualified Data.Graph.Backend.NEC as NEC
import           Data.Graph.Model.Pointer.Set  (RefSet)
import           Data.Container                (usedIxes)
import           Data.Container.SizeTracking   (SizeTracking)
import           Data.Layer_OLD.Cover_OLD
import           Data.Index                    (idx)
import           Data.Prop
import           Data.Map                      (Map)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Map                      as Map
import qualified Data.IntSet                   as IntSet
import           Luna.Syntax.Term.Function      (Signature)

import           Luna.Syntax.Model.Layer
import qualified Luna.Syntax.Term.Function       as Function

#define ImportCtx  ( node  ~ (ls :<: term)                            \
                   , edge  ~ Link node                                \
                   , graph ~ Hetero (NEC.Graph n e c)                 \
                   , BiCastable e edge                                \
                   , BiCastable n node                                \
                   , MonadBuilder graph m                             \
                   , Covered node                                     \
                   , Constructor m (Ref Node node)                    \
                   , Constructor m (Ref Edge edge)                    \
                   , Connectible (Ref Node node) (Ref Node node) m    \
                   , HasInputs (term ls) (Ref Edge edge)              \
                   , HasProp Type   node                              \
                   , HasProp TCData node                              \
                   , HasProp Succs  node                              \
                   , Prop Type   node ~ Ref Edge edge                 \
                   , Prop TCData node ~ TCDataPayload node            \
                   , Prop Succs  node ~ SizeTracking IntSet.IntSet    \
                   , ReferencedM Node graph m node                    \
                   , ReferencedM Edge graph m edge                    \
                   )
                   -- , Referred Node graph n                            \

type NodeTranslator n = Ref Node n -> Ref Node n

mkNodeTranslator :: Map (Ref Node n) (Ref Node n) -> NodeTranslator n
mkNodeTranslator m r = case Map.lookup r m of
    Just res -> res
    Nothing  -> r

translateSignature :: NodeTranslator a -> Signature (Ref Node a) -> Signature (Ref Node a)
translateSignature f sig = sig & Function.self . mapped          %~ f
                               & Function.args . mapped . mapped %~ f
                               & Function.out                    %~ f

importStructure :: ImportCtx => [(Ref Node node, node)] -> [(Ref Edge edge, edge)] -> m (NodeTranslator node)
importStructure nodes' edges' = do
    let nodes           = filter ((/= universe) . fst) nodes'
        edges           = filter ((/= universe) . view target . snd) edges'
        foreignNodeRefs = fst <$> nodes
        foreignEdgeRefs = fst <$> edges

    newNodeRefs <- mapM (construct . (prop Succs .~ fromList []) . (prop TCData . belongsTo .~ [])) $ snd <$> nodes

    let nodeTrans         = Map.fromList $ zip foreignNodeRefs newNodeRefs
        translateNode     = mkNodeTranslator nodeTrans
        translateEdgeEnds = (source %~ translateNode) . (target %~ translateNode)
        foreignEs         = snd <$> edges
        es                = translateEdgeEnds <$> foreignEs

    newEdgeRefs <- forM es $ \e -> connection (e ^. source) (e ^. target)
    let edgeTrans = Map.fromList $ zip foreignEdgeRefs newEdgeRefs

    forM_ newNodeRefs $ \ref -> do
        node <- read ref
        let nodeWithFixedEdges = node & over covered     (fmapInputs unsafeTranslateEdge)
                                      & over (prop Type) unsafeTranslateEdge
                                      & over (prop TCData . redirect  . mapped) unsafeTranslateEdge
                                      & over (prop TCData . requester . mapped) unsafeTranslateEdge
                where
                unsafeTranslateEdge i = fromMaybe i $ Map.lookup i edgeTrans
        write ref nodeWithFixedEdges

    return translateNode

importToCluster :: ( ImportCtx
                   , clus ~ (NetClusterLayers :< RefSet Node node)
                   , Covered clus
                   , Clusterable Node node clus m
                   , BiCastable clus c
                   ) => graph -> m (Ref Cluster clus, NodeTranslator node)
importToCluster g = do
    let foreignNodeRefs = Ptr <$> usedIxes (g ^. wrapped . nodeStore)
        foreignEdgeRefs = Ptr <$> usedIxes (g ^. wrapped . edgeStore)
    foreignNodes <- mapM (flip readRefM g) foreignNodeRefs
    foreignEdges <- mapM (flip readRefM g) foreignEdgeRefs
    trans        <- importStructure (zip foreignNodeRefs foreignNodes) (zip foreignEdgeRefs foreignEdges)
    cls          <- subgraph
    mapM_ (flip include cls) $ filter (/= universe) $ trans <$> foreignNodeRefs
    return (cls, trans)

dupCluster :: ( ImportCtx
              , Getter Inputs node
              , Prop Inputs node ~ [Ref Edge edge]
              , clus ~ (NetClusterLayers :< RefSet Node node)
              , Covered clus
              , HasProp Name   clus
              , HasProp Lambda clus
              , Prop Name   clus ~ String
              , Prop Lambda clus ~ Maybe (Signature (Ref Node node))
              , Clusterable Node node clus m
              , BiCastable clus c
              ) => Ref Cluster clus -> String -> m (Ref Cluster clus, NodeTranslator node)
dupCluster cluster name = do
    nodeRefs <- members cluster
    nodes <- mapM read nodeRefs
    let gatherEdges n = IntSet.fromList $ view idx <$> [n ^. prop Type]
                                                    ++ maybeToList (n ^. prop TCData . redirect)
                                                    ++ maybeToList (n ^. prop TCData . requester)
                                                    ++ (n # Inputs)
    let edgeRefs = Ptr <$> (IntSet.toList $ foldr IntSet.union mempty (gatherEdges <$> nodes))
    edges <- mapM read edgeRefs
    trans <- importStructure (zip nodeRefs nodes) (zip edgeRefs edges)
    fptr  <- follow (prop Lambda) cluster
    cl <- subgraph
    withRef cl $ (prop Name   .~ name)
               . (prop Lambda .~ (translateSignature trans <$> fptr))
    mapM_ (flip include cl) $ trans <$> nodeRefs
    return (cl, trans)

universe :: Ref Node n
universe = Ptr 0
