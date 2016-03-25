{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Cluster where

import Prologue

import Data.Container
import Data.Prop
import Data.Graph.Model.Ref

-- === Abstraction === --

data Cluster = Cluster deriving (Show, Eq, Ord)


class Clustered t where
    clusters :: Lens' t [t # Cluster]

class Monad m => RefContainer c ref m where
    includeRef :: ref -> c -> m c
    excludeRef :: ref -> c -> m c
    toRefList  :: c -> m [ref]

instance {-# OVERLAPPABLE #-} (Covered c, RefContainer (Uncovered c) ref m) => RefContainer c ref m where
    includeRef ref c = do
        new <- includeRef ref $ uncover c
        return $ c & covered .~ new

    excludeRef ref c = do
        new <- excludeRef ref $ uncover c
        return $ c & covered .~ new

    toRefList      = toRefList . uncover

class Clusterable r e c m where
    include :: Ref r e -> Ref Cluster c -> m ()
    exclude :: Ref r e -> Ref Cluster c -> m ()
    members :: Ref Cluster c -> m [Ref r e]

-- === Utils === --

clusterStore :: HasStore Cluster a => Lens' a (Store Cluster a)
clusterStore = store (Proxy :: Proxy Cluster)
