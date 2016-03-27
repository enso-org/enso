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
import Data.Container                 hiding (addM, removeM)
import qualified Data.Container       as Cont
import Data.Graph
import Data.Graph.Builder.Class       hiding (with)
import Data.Layer


-- === Utils === --

withM :: (MonadBuilder t m, ReferencedM r t m a) => Ref r a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: (MonadBuilder t m, ReferencedM r t m a) => Ref r a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

withRef :: (MonadBuilder t m, ReferencedM r t m a) => Ref r a -> (a -> a) -> m ()
withRef = with

follow :: (MonadBuilder t m, ReferencedM r t m a) => Lens' a b -> Ref r a -> m b
follow f ptr = view f <$> read ptr

-- === Reconnects === --

class Reconnectible m r el edgeStore inpStore where
    reconnect :: Lens' el edgeStore -> Ref r el -> inpStore -> m edgeStore

instance (MonadBuilder t m, ReferencedM r t m el, Destructor m (Ref Edge conn), Connectible' (Ref r inp) (Ref r el) m conn)
      => Reconnectible m r el (Ref Edge conn) (Ref r inp) where
    reconnect lens elRef input = do
        el  <- read elRef
        destruct $ el ^. lens
        conn <- connection input elRef
        write elRef $ el & lens .~ conn
        return conn

instance (MonadBuilder t m, ReferencedM r t m el, Destructor m connRef, Connectible' (Ref r inp) (Ref r el) m conn, connRef ~ Ref Edge conn, Traversable f)
      => Reconnectible m r el (f (Ref Edge conn)) (f (Ref r inp)) where
    reconnect lens elRef inputs = do
        el <- read elRef
        mapM_ destruct $ el ^. lens
        conns <- mapM (flip connection elRef) inputs
        write elRef $ el & lens .~ conns
        return conns



--read :: (MonadBuilder t m, Referred r t a) => Ref r a -> m a
--read ref = view (focus ref) <$> get

read :: (MonadBuilder g m, PointedM t tgt g m a) => Ptr t tgt -> m a
read ptr = readPtrM ptr =<< get

write :: (MonadBuilder g m, PointedM t tgt g m a) => Ptr t tgt -> a -> m ()
write = modifyM_ ∘∘ writePtrM

--write :: (MonadBuilder t m, Referred r t a) => Ref r a -> a -> m ()
--write ref = modify_ ∘ set (focus ref)

-- === Instances === --

-- Construction

instance Constructor m (Ref r a) => LayerConstructor m (Ref r a) where
    constructLayer = construct ; {-# INLINE constructLayer #-}

instance (MonadBuilder g m, DynamicM t g m a) => Constructor m (Ref t a) where
    construct = modifyM ∘ addM ; {-# INLINE construct #-}

-- Unregistering

instance (MonadBuilder g m, DynamicM t g m a) => Unregister m (Ref t a) where
    unregister = modifyM_ ∘ removeM ; {-# INLINE unregister #-}

-- Destruction

instance (MonadBuilder t m, Prop Inputs node ~ [inp], ReferencedM Node t m node, Destructor m inp, Getter Inputs node, Destructor m node, Unregister m (Ref Node node), Dispatcher NODE_REMOVE (Ref Node node) m)
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
         , Unregister m (Ref Edge edge)
         , Dispatcher CONNECTION_REMOVE (Ref Edge edge) m)
      => Destructor m (Ref Edge edge) where
    destruct ref = do
        dispatch CONNECTION_REMOVE ref
        unregister ref
    {-# INLINE destruct #-}

--instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
