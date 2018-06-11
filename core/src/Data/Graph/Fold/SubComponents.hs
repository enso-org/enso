{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.SubComponents where

import Prologue hiding (Traversable, Traversal, fold, fold1, traverse)

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.List       as ComponentList
import qualified Data.Graph.Data.Component.Set        as ComponentSet
import qualified Data.Graph.Data.Component.Vector     as ComponentVector
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Fold.Class                as Fold
import qualified Data.Graph.Fold.Scoped               as Fold
import qualified Data.Graph.Fold.Struct               as Fold
import qualified Data.Map.Strict                      as Map
import qualified Data.PtrSet.Mutable                  as PtrSet
import qualified Data.Set                             as Set
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable
import qualified Type.Data.List                       as List

import Data.Generics.Traversable        (GTraversable)
import Data.Graph.Component.Node.Class  (Constructor)
import Data.Graph.Data.Component.Class  (Component)
import Data.Graph.Data.Component.List   (ComponentList)
import Data.Graph.Data.Component.Set    (ComponentSet)
import Data.Graph.Data.Component.Vector (ComponentVector)
import Data.Set                         (Set)
import Data.Vector.Storable.Foreign     (Vector)
import Foreign.Ptr.Utils                (SomePtr)
import Type.Data.Bool                   (Not, type (||))



-----------------------
-- === Discovery === --
-----------------------

-- === Definition === --

data Discovery comp
type instance Fold.Result     (Discovery comp) = ComponentList comp
type instance Fold.LayerScope (Discovery comp) = 'Fold.All


-- === API === --

class SubComponents comp m a where
    subComponents :: a -> m (ComponentList comp)

instance {-# OVERLAPPABLE #-} (Fold.Builder (Discovery comp) m a)
      => SubComponents comp m a where
    subComponents = \a -> Fold.build @(Discovery comp) a $! pure $! mempty
    {-# INLINE subComponents #-}

instance Fold.Builder (Fold.Scoped (Discovery comp)) m
         (Component comp' layout)
      => SubComponents comp m (Component comp' layout) where
    subComponents = \a -> Fold.build @(Fold.Scoped (Discovery comp)) a
             $! pure $! mempty
    {-# INLINE subComponents #-}

instance Monad m => Fold.ComponentBuilder (Discovery comp) m comp'


-- === Struct === --

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder (Fold.Struct (Discovery comp)) m a)
      => Fold.Builder (Discovery comp) m a where
    build = Fold.build @(Fold.Struct (Discovery comp))
    {-# INLINE build #-}


-- === Component === --

instance (Fold.Builder1 (Discovery comp) m (Component comp))
      => Fold.Builder (Discovery comp) m (Component comp layout) where
    build = Fold.build1 @(Discovery comp)
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Builder1 (Discovery comp) m (Component comp') where
    build1 = \_ a -> a
    {-# INLINE build1 #-}

instance Monad m
      => Fold.Builder1 (Discovery comp) m (Component comp) where
    build1 = \comp mr -> (ComponentList.Cons $! Layout.relayout comp) <$> mr
    {-# INLINE build1 #-}

instance Monad m => Fold.Builder (Discovery comp) m (Vector a)


-- === ComponentSet === --

instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Builder1 (Discovery comp) m (ComponentSet comp') where
    build1 = \_ -> id
    {-# INLINE build1 #-}

instance MonadIO m
      => Fold.Builder1 (Discovery comp) m (ComponentSet comp) where
    build1 = \a acc
        -> (\a b -> a <> b) <$> (convert <$> PtrSet.toList (unwrap a)) <*> acc
    {-# INLINE build1 #-}


-- === ComponentVector === --

instance (Fold.Builder1 (Discovery comp) m (ComponentVector comp))
      => Fold.Builder (Discovery comp) m (ComponentVector comp layout) where
    build = Fold.build1 @(Discovery comp)
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Builder1 (Discovery comp) m (ComponentVector comp') where
    build1 = \_ -> id
    {-# INLINE build1 #-}

instance MonadIO m
      => Fold.Builder1 (Discovery comp) m (ComponentVector comp) where
    build1 = \a acc
        -> (\a b -> a <> b) <$> (convert <$> ComponentVector.toList a) <*> acc
    {-# INLINE build1 #-}

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder1 (Fold.Struct (Discovery comp)) m a)
      => Fold.Builder1 (Discovery comp) m a where
    build1 = Fold.build1 @(Fold.Struct (Discovery comp)) ; {-# INLINE build1 #-}
