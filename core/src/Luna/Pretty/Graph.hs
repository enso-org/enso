{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

module Luna.Pretty.Graph where

import Prelude.Luna hiding (id)

import           Data.Aeson       (ToJSON(toJSON, toEncoding), FromJSON, encode, decode, defaultOptions, genericToJSON, genericToEncoding, genericParseJSON)
import           Data.Aeson.Types (fieldLabelModifier)
import qualified Data.Aeson.Types as Aeson
import           Data.Int         (Int64)
import           Data.Text.Lazy   (Text)
import           Data.Char        (toLower, isUpper)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Control.Monad.State.Dependent (MonadState, StateT, modify_)
import qualified Control.Monad.State.Dependent as State



----------------------------------
-- === Visualisation graphs === --
----------------------------------

type Name   = Text
type ID     = Int64
type NodeID = ID
type EdgeID = ID
type Style  = Text

data V = V -- FIXME[WD]: remove after moving dependent state to @-index
type Vis     = Desc Step
type VisDiff = Desc StepDiff

data Desc a = Desc { _nodes :: Map NodeID Node
                   , _edges :: Map EdgeID Edge
                   , _steps :: [a]
                   } deriving (Show, Generic)

data Node = Node { _node_name   :: Name
                 , _node_uid    :: NodeID
                 , _node_id     :: NodeID
                 , _node_styles :: Set Style
                 } deriving (Show, Generic)

data Edge = Edge { _edge_name   :: Name
                 , _edge_uid    :: EdgeID
                 , _edge_id     :: EdgeID
                 , _edge_src    :: NodeID
                 , _edge_tgt    :: NodeID
                 , _edge_styles :: Set Style
                 } deriving (Show, Generic)

data Step = Step { _step_name  :: Name
                 , _stepNodes :: Set NodeID
                 , _stepEdges :: Set EdgeID
                 } deriving (Show, Generic)

data StepDiff = StepDiff { _stepDiff_name    :: Name
                         , _stepDiff_mkNodes :: Set NodeID
                         , _stepDiff_mkEdges :: Set EdgeID
                         , _stepDiff_rmNodes :: Set NodeID
                         , _stepDiff_rmEdges :: Set NodeID
                         } deriving (Show, Generic)

makeLenses ''Desc
makeLenses ''Node
makeLenses ''Edge
makeLenses ''Step
makeLenses ''StepDiff


-- === Classes === --

type family   Diff a


class Diffable a where diff :: a -> Diff a

class HasName      a where name    :: Lens' a Name
class HasUID       a where uid     :: Lens' a ID
class HasID        a where id      :: Lens' a ID
class HasStyles    a where styles  :: Lens' a (Set Style)
class HasSource    a where src     :: Lens' a ID
class HasTarget    a where tgt     :: Lens' a ID
class StepDiffLike a where mkNodes :: Lens' a (Set NodeID)
                           mkEdges :: Lens' a (Set EdgeID)
                           rmNodes :: Lens' a (Set NodeID)
                           rmEdges :: Lens' a (Set EdgeID)


-- === Utils === --

defStep :: Name -> Step
defStep     name = Step     name def def         ; {-# INLINE defStep     #-}

defStepDiff :: Name -> StepDiff
defStepDiff name = StepDiff name def def def def ; {-# INLINE defStepDiff #-}

dropPrefix :: String -> String
dropPrefix ('_': s) = s ; {-# INLINE dropPrefix #-}

paramName :: String -> String
paramName = paramName' . dropPrefix where
    paramName' ('_' : ss) = ss
    paramName' (s   : ss) = paramName' ss
    {-# INLINE paramName' #-}
{-# INLINE paramName #-}


-- === Instances === --

-- Accessors

instance HasName      Node     where name    = node_name        ; {-# INLINE name    #-}
instance HasUID       Node     where uid     = node_uid         ; {-# INLINE uid     #-}
instance HasID        Node     where id      = node_id          ; {-# INLINE id      #-}
instance HasStyles    Node     where styles  = node_styles      ; {-# INLINE styles  #-}

instance HasName      Edge     where name    = edge_name        ; {-# INLINE name    #-}
instance HasUID       Edge     where uid     = edge_uid         ; {-# INLINE uid     #-}
instance HasID        Edge     where id      = edge_id          ; {-# INLINE id      #-}
instance HasSource    Edge     where src     = edge_src         ; {-# INLINE src     #-}
instance HasTarget    Edge     where tgt     = edge_tgt         ; {-# INLINE tgt     #-}
instance HasStyles    Edge     where styles  = edge_styles      ; {-# INLINE styles  #-}

instance HasName      Step     where name    = step_name        ; {-# INLINE name    #-}

instance HasName      StepDiff where name    = stepDiff_name    ; {-# INLINE name    #-}
instance StepDiffLike StepDiff where mkNodes = stepDiff_mkNodes ; {-# INLINE mkNodes #-}
                                     mkEdges = stepDiff_mkEdges ; {-# INLINE mkEdges #-}
                                     rmNodes = stepDiff_rmNodes ; {-# INLINE rmNodes #-}
                                     rmEdges = stepDiff_rmEdges ; {-# INLINE rmEdges #-}

-- Defaults

instance Default (Desc a) where def = Desc  def def def ; {-# INLINE def #-}

-- Diffs

type instance Diff [Step] = [StepDiff]
type instance Diff Vis    = VisDiff

instance Diffable Vis    where diff = steps %~ (reverse . diff . reverse) ; {-# INLINE diff #-}
instance Diffable [Step] where diff []       = []
                               diff [f]      = return $ defStepDiff (f ^. name) & mkNodes .~ (f ^. stepNodes)
                                                                                & mkEdges .~ (f ^. stepEdges)
                               diff (f:s:ss) = d : diff (s:ss) where
                                   d = defStepDiff (f ^. name)
                                     & mkNodes .~ Set.difference (s ^. stepNodes) (f ^. stepNodes)
                                     & rmNodes .~ Set.difference (f ^. stepNodes) (s ^. stepNodes)
                                     & mkEdges .~ Set.difference (s ^. stepEdges) (f ^. stepEdges)
                                     & rmEdges .~ Set.difference (f ^. stepEdges) (s ^. stepEdges)
                               {-# INLINE diff #-}

-- JSON

optsParam :: Aeson.Options
optsParam = defaultOptions { fieldLabelModifier = paramName }

optsPfx :: Aeson.Options
optsPfx = defaultOptions { fieldLabelModifier = dropPrefix }

instance ToJSON a   => ToJSON   (Desc a) where toJSON     = genericToJSON     optsPfx   ; {-# INLINE toJSON     #-}
                                               toEncoding = genericToEncoding optsPfx   ; {-# INLINE toEncoding #-}
instance FromJSON a => FromJSON (Desc a) where parseJSON  = genericParseJSON  optsPfx   ; {-# INLINE parseJSON  #-}

instance ToJSON   Node     where toJSON     = genericToJSON     optsParam ; {-# INLINE toJSON     #-}
                                 toEncoding = genericToEncoding optsParam ; {-# INLINE toEncoding #-}
instance FromJSON Node     where parseJSON  = genericParseJSON  optsParam ; {-# INLINE parseJSON  #-}

instance ToJSON   Edge     where toJSON     = genericToJSON     optsParam ; {-# INLINE toJSON     #-}
                                 toEncoding = genericToEncoding optsParam ; {-# INLINE toEncoding #-}
instance FromJSON Edge     where parseJSON  = genericParseJSON  optsParam ; {-# INLINE parseJSON  #-}

instance ToJSON   Step     where toJSON     = genericToJSON     optsParam ; {-# INLINE toJSON     #-}
                                 toEncoding = genericToEncoding optsParam ; {-# INLINE toEncoding #-}
instance FromJSON Step     where parseJSON  = genericParseJSON  optsParam ; {-# INLINE parseJSON  #-}

instance ToJSON   StepDiff where toJSON     = genericToJSON     optsParam ; {-# INLINE toJSON     #-}
                                 toEncoding = genericToEncoding optsParam ; {-# INLINE toEncoding #-}
instance FromJSON StepDiff where parseJSON  = genericParseJSON  optsParam ; {-# INLINE parseJSON  #-}


-- === Styles === --

literal = "literal"
value   = "value"
thunk   = "thunk"
phrase  = "phrase"
draft   = "draft"

star    = "star"
unify   = "unify"
missing = "missing"
acc     = "acc"

tp      = "type"




------------------------
-- === VisBuilder === --
------------------------

data VisBuilder = VisBuilder { _vis         :: Vis
                             , _currentStep :: Step
                             } deriving (Show, Generic)

makeLenses ''VisBuilder


defVisBuilder :: Name -> VisBuilder
defVisBuilder = VisBuilder def . defStep ; {-# INLINE defVisBuilder #-}



-----------------------
-- === Vis monad === --
-----------------------

-- === Definitions === --

type VisStateT   = StateT     V VisBuilder
type MonadVis  m = MonadState V VisBuilder m


-- === Running === ---

finish :: VisBuilder -> Vis
finish = view vis . newStep' "finish" ; {-# INLINE finish #-}

runT :: Functor m => VisStateT m a -> VisBuilder -> m (a, Vis)
runT = finish <<∘∘>> State.runT V ; {-# INLINE runT #-}

newRunT :: Functor m => VisStateT m a -> m (a, Vis)
newRunT = flip runT $ defVisBuilder "initial" ; {-# INLINE newRunT #-}

newRunDiffT :: Functor m => VisStateT m a -> m (a, VisDiff)
newRunDiffT = diff <<∘>> newRunT ; {-# INLINE newRunDiffT #-}


-- === Component management === ---

newStep :: MonadVis m => Name -> m ()
newStep n = modify_ V (newStep' n) ; {-# INLINE newStep #-}

newStep' :: Name -> VisBuilder -> VisBuilder
newStep' n v = v & vis.steps %~ (<> [v ^. currentStep])
                 & currentStep .~ defStep n
{-# INLINE newStep' #-}

addNode :: MonadVis m => Node -> m ()
addNode a = modify_ V $ ((vis.nodes)             %~ Map.insert nid a)
                      . ((currentStep.stepNodes) %~ Set.insert nid) where
    nid = a ^. uid

addNodes :: (Traversable f, MonadVis m) => f Node -> m ()
addNodes = mapM_ addNode ; {-# INLINE addNodes #-}


addEdge :: MonadVis m => Edge -> m ()
addEdge a = modify_ V $ ((vis.edges)             %~ Map.insert nid a)
                      . ((currentStep.stepEdges) %~ Set.insert nid) where
    nid = a ^. uid

addEdges :: (Traversable f, MonadVis m) => f Edge -> m ()
addEdges = mapM_ addEdge ; {-# INLINE addEdges #-}


--
-- addStep :: MonadVis m => [Step] -> m ()
-- addStep a = modify_ V (steps %~ (<> a)) ; {-# INLINE addStep #-}
--
-- addNodes :: MonadVis m => [Node] -> m ()
-- addNodes a = modify_ V (nodes %~ (<> a)) ; {-# INLINE addNodes #-}
--
-- addEdges :: MonadVis m => [Edge] -> m ()
-- addEdges a = modify_ V (edges %~ (<> a)) ; {-# INLINE addEdges #-}
--
-- addSteps :: MonadVis m => Step -> m ()
-- addSteps = addStep . (:[]) ; {-# INLINE addSteps #-}
--
-- addNode :: MonadVis m => Node -> m ()
-- addNode = addNodes . (:[]) ; {-# INLINE addNode #-}
--
-- addEdge :: MonadVis m => Edge -> m ()
-- addEdge = addEdges . (:[]) ; {-# INLINE addEdge #-}
