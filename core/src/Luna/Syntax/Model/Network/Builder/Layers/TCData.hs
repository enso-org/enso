{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layers.TCData where

import Prelude.Luna

import           Data.Graph.Builders
import           Control.Monad.Event
import           Data.Graph.Backend.VectorGraph         (SubGraph)
import           Data.Prop
import           Data.Construction
import           Data.Graph.Backend.VectorGraph
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import qualified Luna.Syntax.Term.Function               as Func
import           Luna.Compilation.Error
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph

-- === TCData layer === --

data Origin = Conclusion | Assumption deriving (Eq, Show)

instance {-# OVERLAPPING #-} Ord (Maybe Origin) where
    Nothing           <= _                 = True
    _                 <= Nothing           = False
    (Just Conclusion) <= (Just Assumption) = True
    (Just Assumption) <= (Just Conclusion) = False

data TCDataPayload n = TCDataPayload { _redirect    :: Maybe $ Ref Edge $ Link n
                                     , _replacement :: Maybe $ Ptr Cluster
                                     , _requester   :: Maybe $ Ref Edge $ Link n
                                     , _belongsTo   :: [Ptr Cluster]
                                     , _keep        :: Bool
                                     , _seen        :: Bool
                                     , _tcErrors    :: [TCError $ Ref Node n]
                                     , _origin      :: Maybe Origin
                                     } deriving (Show, Eq)
makeLenses ''TCDataPayload

type instance LayerData (Network ls) TCData t = TCDataPayload (Shelled t)
instance Monad m => Creator m (Layer (Network ls) TCData a) where
    create = return $ Layer $ TCDataPayload def def def def False False def def

instance (Monad m, Destructor m (Ref Edge $ Link (Shelled a)))
      => Destructor m (Layer (Network ls) TCData a) where
    destruct (Layer (TCDataPayload red _ req _ _ _ _ _)) = do
        mapM_ destruct red
        mapM_ destruct req

instance Castable t t' => Castable (TCDataPayload t) (TCDataPayload t') where
    cast (TCDataPayload a b c d e f g h) = TCDataPayload (cast <$> a) b (cast <$> c) d e f (cast <$> g) h

