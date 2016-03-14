{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.Ref where

import Prelude.Luna

import Control.Monad.Event
import Data.List                      (delete)
import Data.Graph.Builders
import Data.Construction
import Data.Prop
import Data.Index
import Data.Container                 hiding (add, remove)
import qualified Data.Container       as Cont
import Data.Graph.Builder.Class       hiding (with)
import Data.Graph.Backend.VectorGraph
import Data.Layer
--import Data.Graph.Model.Dynamic (Dynamic, add, remove)


-- === Utils === --

withM :: (MonadBuilder t m, Referred r t a) => Ref r a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: (MonadBuilder t m, Referred r t a) => Ref r a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

withRef :: (MonadBuilder t m, Referred r t a) => Ref r a -> (a -> a) -> m ()
withRef = with

follow :: (MonadBuilder t m, Referred r t a) => Lens' a b -> Ref r a -> m b
follow f ptr = view f <$> read ptr

-- === Reconnects === --

class Reconnectible m r el store inp where
    reconnect :: Lens' el store -> Ref r el -> Ref r inp -> m store

instance (MonadBuilder t m, Referred r t el, Destructor m (Ref Edge conn), Connectible' (Ref r inp) (Ref r el) m conn)
      => Reconnectible m r el (Ref Edge conn) inp where
    reconnect lens elRef input = do
        el  <- read elRef
        destruct $ el ^. lens
        conn <- connection input elRef
        write elRef $ el & lens .~ conn
        return conn

instance (MonadBuilder t m, Referred r t el, Destructor m connRef, Connectible' (Ref r inp) (Ref r el) m conn, connRef ~ Ref Edge conn)
      => Reconnectible m r el (Maybe connRef) inp where
    reconnect lens elRef input = do
        el  <- read elRef
        mapM_ destruct $ el ^. lens
        conn <- connection input elRef
        write elRef $ el & lens ?~ conn
        return $ Just conn

--class Referred r t a where ref :: Ref r a -> Lens' t a

read :: (MonadBuilder t m, Referred r t a) => Ref r a -> m a
read ref = view (focus ref) <$> get

write :: (MonadBuilder t m, Referred r t a) => Ref r a -> a -> m ()
write ref = modify_ ∘ set (focus ref)

-- === Instances === --

-- Construction

instance Constructor m (Ref r a) => LayerConstructor m (Ref r a) where
    constructLayer = construct ; {-# INLINE constructLayer #-}

instance (MonadBuilder g m, Dynamic t g a) => Constructor m (Ref t a) where
    construct = modify ∘ add ; {-# INLINE construct #-}

-- Unregistering

instance (MonadBuilder g m, Dynamic t g a) => Unregister m (Ref t a) where
    unregister = modify_ ∘ remove ; {-# INLINE unregister #-}

-- Destruction

instance (MonadBuilder t m, Prop Inputs node ~ [inp], Referred Node t node, Destructor m inp, Getter Inputs node, Destructor m node, Unregister m (Ref Node node), Dispatcher NODE_REMOVE (Ref Node node) m)
      => Destructor m (Ref Node node) where
    destruct ref = do
        dispatch NODE_REMOVE ref
        n <- read ref
        mapM_ destruct $ n # Inputs
        destruct n
        unregister ref
    {-# INLINE destruct #-}

instance ( MonadBuilder t m
         , edge ~ Arc node node
         , Referred Node t node
         , Referred Edge t edge
         , Unregister m (Ref Edge edge)
         , Dispatcher CONNECTION_REMOVE (Ref Edge edge) m)
      => Destructor m (Ref Edge edge) where
    destruct ref = do
        dispatch CONNECTION_REMOVE ref
        unregister ref
    {-# INLINE destruct #-}

--instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
