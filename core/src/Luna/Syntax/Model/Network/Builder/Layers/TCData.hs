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
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph

-- === TCData layer === --

data TCDataPayload n = TCDataPayload { _redirect    :: Maybe $ Ref Edge $ Link n
                                     , _replacement :: Maybe $ Ptr Cluster
                                     , _belongsTo   :: [Ptr Cluster]
                                     , _keep        :: Bool
                                     , _seen        :: Bool
                                     } deriving (Show, Eq)
makeLenses ''TCDataPayload

type instance LayerData (Network ls) TCData t = TCDataPayload (Shelled t)
instance Monad m => Creator m (Layer (Network ls) TCData a) where
    create = return $ Layer $ TCDataPayload Nothing Nothing [] False False

instance (Monad m, Unregister m (Ref Edge $ Link (Shelled a)))
      => Destructor m (Layer (Network ls) TCData a) where
    destruct (Layer (TCDataPayload red _ _ _ _)) = mapM_ unregister red

instance Castable t t' => Castable (TCDataPayload t) (TCDataPayload t') where
    cast (TCDataPayload a b c d e) = TCDataPayload (cast <$> a) b c d e

