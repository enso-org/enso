{-# LANGUAGE OverloadedStrings #-}

module Luna.Debug.IR.Visualizer where

import Prologue

import qualified Control.Monad.State        as State
import qualified Control.Lens.Aeson         as Lens
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Tag                   as Tag
import qualified Luna.IR                    as IR
import qualified Luna.IR.Layer              as Layer
import qualified Luna.Pass                  as Pass
import qualified Data.Graph.Component.Layout              as IR
import qualified System.Environment         as System
import qualified Web.Browser                as Browser

import Data.Map (Map)
import Data.Set (Set)

-- === Definitions === --

type NodeId = Int

data Node = Node
    { __label  :: Text
    , __styles :: [Text]
    , __id     :: NodeId
    } deriving (Generic, Show)
makeLenses ''Node

instance Aeson.ToJSON Node where
    toEncoding = Lens.toEncoding

data Edge = Edge
    { __src    :: NodeId
    , __dst    :: NodeId
    , __styles :: [Text]
    } deriving (Generic, Show)
makeLenses ''Edge

instance Aeson.ToJSON Edge where
    toEncoding = Lens.toEncoding

data Graph = Graph
    { __nodes :: [Node]
    , __edges :: [Edge]
    } deriving (Generic, Show)
makeLenses ''Graph

instance Aeson.ToJSON Graph where
    toEncoding = Lens.toEncoding

type MonadVis m =
    ( Monad m
    , MonadIO m
    , Layer.Reader IR.Terms IR.Model  m
    , Layer.Reader IR.Terms IR.Type   m
    , Layer.Reader IR.Links IR.Source m
    )

-- === Private API === --

gatherNodesFrom :: MonadVis m => IR.Term layout -> m (Set IR.SomeTerm)
gatherNodesFrom root = State.execStateT (go $ IR.relayout root) def where
    go (root :: IR.SomeTerm) = do
        visited <- State.gets $ Set.member root
        if visited then return () else do
            model <- Layer.read @IR.Model root
            State.modify $ Set.insert root
            inps <- IR.inputs model >>= traverse IR.source
            tp   <- Layer.read @IR.Type root >>= IR.source
            traverse_ (go . IR.relayout) inps
            go $ IR.relayout tp

buildVisualizationGraph :: MonadVis m => IR.Term layout -> m Graph
buildVisualizationGraph root = do
    allNodes <- gatherNodesFrom root
    let nodesWithIds = Map.fromList $ zip (Set.toList allNodes) [1..]
    visNodes <- traverse (buildVisualizationNode nodesWithIds) $ Map.keys nodesWithIds
    return $ Graph (fst <$> visNodes) (concat $ snd <$> visNodes)


buildVisualizationNode :: MonadVis m
    => Map IR.SomeTerm NodeId -> IR.SomeTerm -> m (Node, [Edge])
buildVisualizationNode idsMap ref = do
    model <- Layer.read @IR.Model ref
    inps  <- IR.inputs model >>= traverse IR.source
    tp    <- Layer.read @IR.Type ref >>= IR.source
    let getNodeId :: forall l. IR.Term l -> NodeId
        getNodeId = unsafeFromJust . flip Map.lookup idsMap . IR.relayout
        tgtId     = getNodeId ref
        tag       = Tag.showTag model
        tpEdge    = Edge (getNodeId tp) tgtId ["type"]
        inpEdges  = (\n -> Edge (getNodeId n) tgtId ["input"]) <$> inps
    return (Node tag [tag] tgtId, tpEdge : inpEdges)

-- === Public API === --

displayVisualization :: MonadVis m => String -> IR.Term layout -> m ()
displayVisualization name root = do
    graph <- buildVisualizationGraph root
    let visData = ByteString.unpack $ Aeson.encode graph
    liftIO $ do
        dataPath  <- System.lookupEnv "VIS_DATA_PATH"
        visUriEnv <- System.lookupEnv "VIS_URI"
        let visUri = fromJust "http://localhost:8000" visUriEnv
        for_ dataPath $ \path -> do
            writeFile (path <> "/" <> name <> ".json") visData
            Browser.openBrowser $ visUri <> "?cfgPath=" <> name
