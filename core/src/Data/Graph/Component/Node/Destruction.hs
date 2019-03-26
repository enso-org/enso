module Data.Graph.Component.Node.Destruction where

import           Prologue hiding (Type)

import qualified Data.Graph.Component.Edge.Class       as Edge
import qualified Data.Graph.Component.Edge.Destruction as Edge
import qualified Data.Graph.Component.Node.Class       as Node
import qualified Data.Graph.Component.Node.Layer       as Node
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Set                              as Set

import Control.Monad                   ( filterM)
import Data.Graph.Component.Edge.Class ( Edge, Edges, Source, Target)
import Data.Graph.Component.Node.Class ( Node, Nodes)
import Data.Graph.Component.Node.Layer ( Model, Type, Users)
import Data.Graph.Fold.SubComponents   ( SubComponents, SubComponents1
                                       , subComponents1)


-------------------------
-- === Destruction === --
-------------------------

-- === API === --

type Delete m =
    ( MonadIO m
    , Edge.Delete m
    , Component.Destructor1 m Node
    , SubComponents1 Edges m Nodes
    )

delete :: Delete m => Node layout -> m ()
delete = \node -> do
    edges <- subComponents1 @Edges node
    let es = Set.toList $ Set.fromList $ (convert edges :: [Edge.SomeEdge])
    traverse Edge.delete es
    Component.destruct1 node
    pure ()
{-# INLINE delete #-}

type DeleteSubtree m =
    ( MonadIO m
    , Delete m
    , Layer.Reader Node Users  m
    , Layer.Reader Node Model  m
    , Layer.Reader Node Type   m
    , Layer.Reader Edge Target m
    , Layer.Reader Edge Source m
    , SubComponents Edges m (Node.Uni ())
    , Layer.IsUnwrapped Node.Uni
    )

safeToDelete :: ∀ m. DeleteSubtree m
             => Set.Set Node.Some -> Node.Some -> m Bool
safeToDelete whitelist root = do
    succs <- Mutable.toList =<< Layer.read @Users root
    loops <- traverse Edge.cyclic succs
    let allLoops    = and loops
        whitelisted = Set.member root whitelist
    pure $ allLoops && not whitelisted
{-# INLINE safeToDelete #-}

deleteSubtreeWithWhitelist :: ∀ layout m. DeleteSubtree m
                           => Set.Set Node.Some -> Node layout -> m ()
deleteSubtreeWithWhitelist whitelist (Layout.relayout -> root) = whenM (safeToDelete whitelist root) $ go root where
    go :: Node.Some -> m ()
    go root = do
        inputEdges :: [Edge.SomeEdge] <- convert <$> Node.inputs root
        tpEdge <- Layer.read @Type root
        let allInputEdges = Layout.relayout tpEdge : inputEdges
        nonCyclicEdges <- filterM (fmap not . Edge.cyclic) allInputEdges
        inputs         <- traverse (Layer.read @Source) nonCyclicEdges
        delete root
        safeChildren <- filterM (safeToDelete whitelist) $ Set.toList $ Set.fromList inputs
        traverse_ go safeChildren
{-# INLINE deleteSubtreeWithWhitelist #-}

deleteSubtree :: DeleteSubtree m => Node layout -> m ()
deleteSubtree = deleteSubtreeWithWhitelist mempty
{-# INLINE deleteSubtree #-}

