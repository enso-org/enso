{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layers.SuccTracking where

import Prelude.Luna

import           Control.Monad.Event
import           Data.Graph.Builders
import           Data.Container.SizeTracking            (SizeTracking)
import           Data.Container                         (Addable, Removable, add, remove, try)
import           Data.IntSet                            (IntSet)
import           Data.Graph                             hiding (add, remove)
import           Old.Data.Prop
import qualified Data.List                              as List
import           Data.Construction
import           Data.Graph.Backend.NEC
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import qualified Luna.Syntax.Term.Function               as Func
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer_OLD.Cover_OLD



--------------------------------
-- === Succs registration === --
--------------------------------

data SuccRegister = SuccRegister deriving (Show, Eq)
instance ( MonadBuilder g m
         , ReferencedM Edge g (Listener t SuccRegister m) (Arc src tgt)
         , ReferencedM Node g (Listener t SuccRegister m) src
         , Show src
         , Prop Succs src ~ SizeTracking IntSet
         , HasProp Succs src
         ) => Handler t SuccRegister m (Ref Edge (Arc src tgt)) where
    handler e = do
        ve <- read e
        Ref.with (ve ^. source) $ prop Succs %~ add (unwrap e)
    {-# INLINE handler #-}

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = runListener



---------------------------
-- === Succs removal === --
---------------------------

data SuccUnregister = SuccUnregister deriving (Show, Eq)
instance ( MonadBuilder g m
         , ReferencedM Edge g (Listener t SuccUnregister m) (Arc src tgt)
         , ReferencedM Node g (Listener t SuccUnregister m) src
         , Show src
         , Prop Succs src ~ SizeTracking IntSet
         , HasProp Succs src
         ) => Handler t SuccUnregister m (Ref Edge (Arc src tgt)) where
    handler e = do
        s <- follow source e
        Ref.with s $ prop Succs %~ try remove (unwrap e)
    {-# INLINE handler #-}

unregisterSuccs :: t -> Listener t SuccUnregister m a -> m a
unregisterSuccs _ = runListener



-------------------------
-- === Layer Data === ---
-------------------------

type instance LayerData Succs t = SizeTracking IntSet

instance Monad m => Creator    m (Layer Succs a) where create     = return $ Layer $ fromList [] ; {-# INLINE create   #-}
instance Monad m => Destructor m (Layer Succs a) where destruct _ = return ()                    ; {-# INLINE destruct #-}

instance Castable IntSet IntSet where cast = id

type HasSuccs n = (HasProp Succs n, Prop Succs n ~ SizeTracking IntSet)



----------------------
-- === Reading === ---
----------------------

readSuccs :: HasSuccs n => n -> [Ref Edge (Link n)]
readSuccs = fmap Ptr . toList . view (prop Succs)
