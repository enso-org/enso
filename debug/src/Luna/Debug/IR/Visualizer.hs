{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Debug.IR.Visualizer where

import Prologue

import qualified Control.Lens.Aeson              as Lens
import qualified Control.Monad.State             as State
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy.Char8      as ByteString
import qualified Data.Graph.Data.Layer.Layout    as IR
import qualified Data.Graph.Data.Component.Class as Graph
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Tag                        as Tag
import qualified Luna.IR                         as IR
import qualified Luna.IR.Layer                   as Layer
import qualified Luna.Pass                       as Pass
import qualified Luna.Pass.Attr                  as Attr
import qualified Luna.Pass.Basic                 as Pass
import qualified Luna.Pass.Scheduler             as Scheduler
import qualified System.Environment              as System
import qualified Web.Browser                     as Browser

import Data.Map (Map)
import Data.Set (Set)


---------------------------
-- === IR Visualizer === --
---------------------------

-- === Definitions === --

type NodeId = Text

data Node = Node
    { __label  :: Text
    , __styles :: [Text]
    , __id     :: NodeId
    } deriving (Generic, Show)
makeLenses ''Node

data Edge = Edge
    { __src    :: NodeId
    , __dst    :: NodeId
    , __label  :: Text
    , __styles :: [Text]
    } deriving (Generic, Show)
makeLenses ''Edge

data Graph = Graph
    { __nodes :: [Node]
    , __edges :: [Edge]
    } deriving (Generic, Show)
makeLenses ''Graph

type MonadVis m =
    ( MonadIO m
    , Layer.Reader IR.Term IR.Model  m
    , Layer.Reader IR.Term IR.Type   m
    , Layer.Reader IR.Link IR.Source m
    , Graph.Allocator IR.Terms m
    )


-- === Private API === --

getAllNodes :: MonadVis m => m [IR.SomeTerm]
getAllNodes = IR.getAllAllocated

gatherNodesFrom :: MonadVis m => IR.Term layout -> m (Set IR.SomeTerm)
gatherNodesFrom root = State.execStateT (go $ IR.relayout root) def where
    go (root :: IR.SomeTerm) = do
        visited <- State.gets $ Set.member root
        when_ (not visited) $ do
            model <- Layer.read @IR.Model root
            inps  <- ComponentList.mapM IR.source =<< IR.inputs root
            tp    <- IR.source =<< Layer.read @IR.Type root
            State.modify $ Set.insert root
            traverse_ (go . IR.relayout) inps
            go $ IR.relayout tp

buildVisualizationGraphFromRoot :: MonadVis m => IR.Term layout -> m Graph
buildVisualizationGraphFromRoot root = do
    allNodes <- Set.toList <$> gatherNodesFrom root
    buildVisualizationGraph allNodes

buildVisualizationGraphFromAll :: MonadVis m => m Graph
buildVisualizationGraphFromAll = do
    allNodes <- getAllNodes
    buildVisualizationGraph allNodes

buildVisualizationGraph :: MonadVis m => [IR.SomeTerm] -> m Graph
buildVisualizationGraph nodes = do
    visNodes <- traverse buildVisualizationNode nodes
    pure $ Graph (fst <$> visNodes) (concat $ snd <$> visNodes)

buildVisualizationNode :: MonadVis m
    => IR.SomeTerm -> m (Node, [Edge])
buildVisualizationNode ref = do
    model <- Layer.read @IR.Model ref
    inps  <- IR.inputs ref
    srcs  <- ComponentList.mapM (\i -> (i,) <$> IR.source i) inps
    tpl   <- Layer.read @IR.Type ref
    tp    <- IR.source tpl
    let getNodeId :: ∀ c layout. Graph.Component c layout -> NodeId
        getNodeId = convert . show . unwrap
        tgtId     = getNodeId ref
        tag       = IR.showTag model
        tpEdge    = Edge (getNodeId tp) tgtId (getNodeId tpl) ["type"]
        inpEdges  = (\(l, n) -> Edge (getNodeId n) tgtId (getNodeId l) ["input"]) <$> srcs
    pure (Node tag [tag] tgtId, tpEdge : inpEdges)

displayGraph :: MonadIO m => String -> Graph -> m ()
displayGraph name graph = liftIO $ do
    let visData = ByteString.unpack $ Aeson.encode graph
    dataPath  <- System.lookupEnv "VIS_DATA_PATH"
    visUriEnv <- System.lookupEnv "VIS_URI"
    let visUri = fromJust "http://localhost:8000" visUriEnv
    for_ dataPath $ \path -> do
        writeFile (path <> "/" <> name <> ".json") visData
        Browser.openBrowser $ visUri <> "?cfgPath=" <> name

-- === Public API === --

visualizeSubtree :: MonadVis m => String -> IR.Term layout -> m ()
visualizeSubtree name root = do
    graph <- buildVisualizationGraphFromRoot root
    displayGraph name graph

visualizeAll :: MonadVis m => String -> m ()
visualizeAll name = do
    graph <- buildVisualizationGraphFromAll
    displayGraph name graph

-- === Pass === --

newtype VisRoot = VisRoot IR.SomeTerm
type instance Attr.Type VisRoot = Attr.Atomic
instance Default VisRoot where
    def = VisRoot Graph.unsafeNull

newtype VisName = VisName String
type instance Attr.Type VisName = Attr.Atomic
instance Default VisName where
    def = VisName ""

data Visualizer
type instance Pass.Spec Visualizer t = VisualizerSpec t
type family VisualizerSpec t where
    VisualizerSpec (Pass.In  Pass.Attrs) = '[VisRoot, VisName]
    VisualizerSpec (Pass.Out Pass.Attrs) = '[]
    VisualizerSpec t = Pass.BasicPassSpec t

instance Pass.Interface Visualizer (Pass.Pass stage Visualizer)
      => Pass.Definition stage Visualizer where
    definition = do
        VisName n <- Attr.get
        VisRoot r <- Attr.get
        visualizeSubtree n r

runPass :: forall stage m.
    ( Scheduler.MonadScheduler m
    , Scheduler.PassRegister stage Visualizer m
    , Pass.Definition stage Visualizer
    ) => String -> IR.SomeTerm -> m ()
runPass n r = do
    Scheduler.registerPass @stage @Visualizer

    Scheduler.registerAttr @VisName
    Scheduler.registerAttr @VisRoot

    Scheduler.setAttr $ VisName n
    Scheduler.setAttr $ VisRoot r

    Scheduler.runPassByType @Visualizer


-- === Instances === --

instance Aeson.ToJSON Node  where toEncoding = Lens.toEncoding
instance Aeson.ToJSON Edge  where toEncoding = Lens.toEncoding
instance Aeson.ToJSON Graph where toEncoding = Lens.toEncoding
