{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

module OCI.IR.Repr.Vis.Class where

import Luna.Prelude hiding (id)

import           Data.Aeson       (ToJSON(toJSON, toEncoding), FromJSON, encode, decode, defaultOptions, genericToJSON, genericToEncoding, genericParseJSON)
import           Data.Aeson.Types (fieldLabelModifier)
import qualified Data.Aeson.Types as Aeson
import           Data.Int         (Int64)
import           Data.Char        (toLower, isUpper)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Luna.IR.Layer.UID (ID)

import           Control.Monad.State.Dependent.Old (MonadState, StateT, modify_)
import qualified Control.Monad.State.Dependent.Old as State

import Data.Aeson       (GFromJSON, GToJSON, GToEncoding, Zero, Value, Encoding)
import Data.Aeson.Types (Parser)
import GHC.Generics     (Rep)



-----------------------------------
-- === Visualisatiobn graphs === --
-----------------------------------

type Name   = Text
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

data Node = Node { __name   :: Name
                 , __uid    :: NodeID
                 , __id     :: NodeID
                 , __styles :: Set Style
                 , __label  :: Maybe Text
                 } deriving (Show, Generic)

data Edge = Edge { __name   :: Name
                 , __uid    :: EdgeID
                 , __id     :: EdgeID
                 , __src    :: NodeID
                 , __tgt    :: NodeID
                 , __styles :: Set Style
                 , __label  :: Maybe Text
                 } deriving (Show, Generic)

data Step = Step { __name      :: Name
                 , __stepNodes :: Set NodeID
                 , __stepEdges :: Set EdgeID
                 } deriving (Show, Generic)

data StepDiff = StepDiff { __name    :: Name
                         , __mkNodes :: Set NodeID
                         , __mkEdges :: Set EdgeID
                         , __rmNodes :: Set NodeID
                         , __rmEdges :: Set NodeID
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

dropLensPrefix :: String -> String
dropLensPrefix ('_':'_':s) = s
dropLensPrefix ('_':s)     = s

-- FIXME[WD]: refactor vvv

genericLensToJSON     :: (Generic a, GToJSON     Zero (Rep a)) => a -> Value
genericLensToEncoding :: (Generic a, GToEncoding Zero (Rep a)) => a -> Encoding
genericLensParseJSON  :: (Generic a, GFromJSON   Zero (Rep a)) => Value -> Parser a
genericLensToJSON     = genericToJSON     optsPfx
genericLensToEncoding = genericToEncoding optsPfx
genericLensParseJSON  = genericParseJSON  optsPfx


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
                               diff [f]      = return $ defStepDiff (f ^. name) & mkNodes .~ (f ^. step_stepNodes)
                                                                                & mkEdges .~ (f ^. step_stepEdges)
                               diff (f:s:ss) = d : diff (s:ss) where
                                   d = defStepDiff (f ^. name)
                                     & mkNodes .~ Set.difference (f ^. step_stepNodes) (s ^. step_stepNodes)
                                     & rmNodes .~ Set.difference (s ^. step_stepNodes) (f ^. step_stepNodes)
                                     & mkEdges .~ Set.difference (f ^. step_stepEdges) (s ^. step_stepEdges)
                                     & rmEdges .~ Set.difference (s ^. step_stepEdges) (f ^. step_stepEdges)
                               {-# INLINE diff #-}

-- JSON

optsPfx :: Aeson.Options
optsPfx = defaultOptions { fieldLabelModifier = dropLensPrefix }


instance ToJSON a   => ToJSON   (Desc a) where toJSON     = genericLensToJSON     ; {-# INLINE toJSON     #-}
                                               toEncoding = genericLensToEncoding ; {-# INLINE toEncoding #-}
instance FromJSON a => FromJSON (Desc a) where parseJSON  = genericLensParseJSON  ; {-# INLINE parseJSON  #-}

instance ToJSON   Node     where toJSON     = genericLensToJSON     ; {-# INLINE toJSON     #-}
                                 toEncoding = genericLensToEncoding ; {-# INLINE toEncoding #-}
instance FromJSON Node     where parseJSON  = genericLensParseJSON  ; {-# INLINE parseJSON  #-}

instance ToJSON   Edge     where toJSON     = genericLensToJSON     ; {-# INLINE toJSON     #-}
                                 toEncoding = genericLensToEncoding ; {-# INLINE toEncoding #-}
instance FromJSON Edge     where parseJSON  = genericLensParseJSON  ; {-# INLINE parseJSON  #-}

instance ToJSON   Step     where toJSON     = genericLensToJSON     ; {-# INLINE toJSON     #-}
                                 toEncoding = genericLensToEncoding ; {-# INLINE toEncoding #-}
instance FromJSON Step     where parseJSON  = genericLensParseJSON  ; {-# INLINE parseJSON  #-}

instance ToJSON   StepDiff where toJSON     = genericLensToJSON     ; {-# INLINE toJSON     #-}
                                 toEncoding = genericLensToEncoding ; {-# INLINE toEncoding #-}
instance FromJSON StepDiff where parseJSON  = genericLensParseJSON  ; {-# INLINE parseJSON  #-}


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

--FIXME[WD]: Make a real Vis monad instead of alias for dependent state in order to make beautiful type inference

-- === Definitions === --

type VisStateT   = StateT     V Vis
type MonadVis  m = MonadState V Vis m


-- === Running === ---

runT :: Functor m => VisStateT m a -> Vis -> m (a, Vis)
runT = State.runT V ; {-# INLINE runT #-}

newRunT :: Functor m => VisStateT m a -> m (a, Vis)
newRunT = flip runT def ; {-# INLINE newRunT #-}

newRunDiffT :: Functor m => VisStateT m a -> m (a, VisDiff)
newRunDiffT = diff .:. newRunT ; {-# INLINE newRunDiffT #-}


addStep :: MonadVis m => Name -> [Node] -> [Edge] -> m ()
addStep title ns es = do
    mapM_ registerNode ns
    mapM_ registerEdge es
    let newStep = Step title (Set.fromList $ view uid <$> ns) (Set.fromList $ view uid <$> es)
    modify_ V $ steps %~ (<> [newStep])

registerNode :: MonadVis m => Node -> m ()
registerNode a = modify_ V $ nodes %~ Map.insert (a ^. uid) a ; {-# INLINE registerNode #-}

registerEdge :: MonadVis m => Edge -> m ()
registerEdge a = modify_ V $ edges %~ Map.insert (a ^. uid) a ; {-# INLINE registerEdge #-}
