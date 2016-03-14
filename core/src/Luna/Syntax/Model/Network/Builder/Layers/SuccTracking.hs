{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layers.SuccTracking where

import Prelude.Luna

import           Data.Graph.Builders
import           Control.Monad.Event
import           Data.Graph.Backend.VectorGraph         (SubGraph)
import           Data.Prop
import qualified Data.List                              as List
import           Data.Construction
import           Data.Graph.Backend.VectorGraph
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import qualified Luna.Syntax.AST.Function               as Func
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph

--------------------------------
-- === Succs registration === --
--------------------------------

data SuccRegister = SuccRegister deriving (Show, Eq)
instance ( MonadBuilder g m
         , Referred Edge g (Arc src tgt)
         , Referred Node g src
         , Show src
         , Prop Succs src ~ [Ref Edge (Arc src tgt)]
         , HasProp Succs src
         ) => Handler t SuccRegister m (Ref Edge (Arc src tgt)) where
    handler e = do
        ve <- read e
        Ref.with (ve ^. source) $ prop Succs %~ (e:)
    {-# INLINE handler #-}

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = runListener

---------------------------
-- === Succs removal === --
---------------------------

data SuccUnregister = SuccUnregister deriving (Show, Eq)
instance ( MonadBuilder g m
         , Referred Edge g (Arc src tgt)
         , Referred Node g src
         , Show src
         , Prop Succs src ~ [Ref Edge (Arc src tgt)]
         , HasProp Succs src
         ) => Handler t SuccUnregister m (Ref Edge (Arc src tgt)) where
    handler e = do
        s <- follow source e
        Ref.with s $ prop Succs %~ List.delete e
    {-# INLINE handler #-}

unregisterSuccs :: t -> Listener t SuccUnregister m a -> m a
unregisterSuccs _ = runListener

-------------------------
-- === Layer Data === ---
-------------------------

type instance LayerData (Network ls) Succs t = [Ref Edge $ Link (Shelled t)]
instance Monad m => Creator    m (Layer (Network ls) Succs a) where
    create     = return $ Layer []
instance Monad m => Destructor m (Layer (Network ls) Succs a) where
    destruct _ = return ()

