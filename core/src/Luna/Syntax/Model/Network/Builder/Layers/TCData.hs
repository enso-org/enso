{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layers.TCData where

import Prelude.Luna

import           Data.Graph.Builders
import           Control.Monad.Event
import           Data.Prop
import           Data.Construction
import           Data.Graph.Backend.RefSet
import           Data.Graph.Backend.NEC
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

data TCDataPayload n = TCDataPayload { _redirect    :: Maybe $ Ref Edge (Link n)
                                     , _requester   :: Maybe $ Ref Edge (Link n)
                                     , _tcErrors    :: [TCError $ Ref Node n]
                                     , _replacement :: Maybe $ Loc Cluster
                                     , _belongsTo   :: [Loc Cluster]
                                     , _depth       :: Maybe Int
                                     , _seen        :: Bool
                                     } deriving (Show, Eq)
makeLenses ''TCDataPayload

type instance LayerData (Network ls) TCData t = TCDataPayload (Shelled t)
instance Monad m => Creator m (Layer (Network ls) TCData a) where
    create = return $ Layer $ TCDataPayload def def def def def def False

instance (Monad m, Destructor m (Ref Edge (Link (Shelled a))))
      => Destructor m (Layer (Network ls) TCData a) where
    destruct (Layer (TCDataPayload red req _ _ _ _ _)) = do
        mapM_ destruct red
        mapM_ destruct req

instance Castable t t' => Castable (TCDataPayload t) (TCDataPayload t') where
    cast (TCDataPayload red req tce rep bto dep seen) = TCDataPayload (cast <$> red) (cast <$> req) (cast <$> tce) rep bto dep seen
