{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.Ref where

import Prelude.Luna

import Data.List                      (delete)
import Data.Graph.Builders
import Data.Graph
import Data.Construction
import Data.Prop
import Data.Index
import Data.Container
import Data.Graph.Builder.Class       hiding (with)
import Data.Graph.Backend.VectorGraph
import Data.Graph.Model


-- === Utils === --

--type RefHandler r m a = (Reader r m a, Writer r m a)
--class Monad m => Reader r m a where read  :: Ref r a -> m a
--class Monad m => Writer r m a where write :: Ref r a -> a -> m ()


withM :: (MonadBuilder t m, Referred r a t) => Ref r a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: (MonadBuilder t m, Referred r a t) => Ref r a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

withRef :: (MonadBuilder t m, Referred r a t) => Ref r a -> (a -> a) -> m ()
withRef = with

follow :: (MonadBuilder t m, Referred r a t) => Lens' a b -> Ref r a -> m b
follow f ptr = view f <$> read ptr

-- === Reconnects === --

class Reconnectible m r el store inp where
    reconnect :: Ref r el -> Lens' el store -> Ref r inp -> m store

instance (MonadBuilder t m, Referred r el t, Destructor m (Ref Edge conn), Connectible' (Ref r inp) (Ref r el) m conn)
      => Reconnectible m r el (Ref Edge conn) inp where
    reconnect elRef lens input = do
        el  <- read elRef
        destruct $ el ^. lens
        conn <- connection input elRef
        write elRef $ el & lens .~ conn
        return conn

instance (MonadBuilder t m, Referred r el t, Destructor m connRef, Connectible' (Ref r inp) (Ref r el) m conn, connRef ~ Ref Edge conn)
      => Reconnectible m r el (Maybe connRef) inp where
    reconnect elRef lens input = do
        el  <- read elRef
        mapM_ destruct $ el ^. lens
        conn <- connection input elRef
        write elRef $ el & lens ?~ conn
        return $ Just conn

--class Referred r a t where ref :: Ref r a -> Lens' t a

read :: (MonadBuilder t m, Referred r a t) => Ref r a -> m a
read ref = view (focus ref) <$> get

write :: (MonadBuilder t m, Referred r a t) => Ref r a -> a -> m ()
write ref = modify_ ∘ set (focus ref)

-- === Instances === --

-- Construction

instance (MonadBuilder (Hetero (VectorGraph n e c)) m, Castable a n) => Constructor m (Ref Node a) where
    construct n = Ref <$> modify (wrapped' ∘ nodeGraph $ swap ∘ ixed add (cast n)) ; {-# INLINE construct #-}

instance (MonadBuilder (Hetero (VectorGraph n e c)) m, Castable a e) => Constructor m (Ref Edge a) where
    construct e = Ref <$> modify (wrapped' ∘ edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}

instance (MonadBuilder (Hetero (VectorGraph n e c)) m, Castable a c) => Constructor m (Ref Cluster a) where
    construct c = Ref <$> modify (wrapped' ∘ subGraphs $ swap ∘ ixed add (cast c)) ; {-# INLINE construct #-}

-- Accessors

--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable n a)              => Reader r m (Node a)       where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e e')             => Reader r m (Edge e')      where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
--instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Reader r m Cluster        where read  = flip fmap get ∘ getter ; # INLINE read #

--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a n)              => Writer r m (Node a)       where write = modify_ ∘∘ setter ; {-# INLINE write #-}
--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e' e)             => Writer r m (Edge e')      where write = modify_ ∘∘ setter ; {-# INLINE write #-}
--instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Writer r m Cluster        where write = modify_ ∘∘ setter ; {-# INLINE write #-}

-- Unregistering

-- FXIME[WD]: hardcoded graph type
instance MonadBuilder (Hetero (VectorGraph n e c)) m => Unregister m (Ref Node    node)    where unregister ref = modify_ $ wrapped' ∘ nodeGraph %~ free (ref ^. idx)
instance MonadBuilder (Hetero (VectorGraph n e c)) m => Unregister m (Ref Edge    edge)    where unregister ref = modify_ $ wrapped' ∘ edgeGraph %~ free (ref ^. idx)
instance MonadBuilder (Hetero (VectorGraph n e c)) m => Unregister m (Ref Cluster cluster) where unregister ref = modify_ $ wrapped' ∘ subGraphs %~ free (ref ^. idx)

-- Destruction

instance (MonadBuilder t m, Prop Inputs node ~ [inp], Referred Node node t, Destructor m inp, Getter Inputs node, Destructor m node, Unregister m (Ref Node node))
      => Destructor m (Ref Node node) where
    destruct ref = do
        n <- read ref
        mapM_ destruct $ n # Inputs
        destruct n
        unregister ref
    {-# INLINE destruct #-}

instance (MonadBuilder t m, Prop Succs node ~ [Ref Edge edge], edge ~ Arc node node, HasProp Succs node, Referred Node node t, Referred Edge edge t, Unregister m (Ref Edge edge))
      => Destructor m (Ref Edge edge) where
    destruct ref = do
        s <- follow source ref
        withRef s $ prop Succs %~ delete ref
        unregister ref
    {-# INLINE destruct #-}

--instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
