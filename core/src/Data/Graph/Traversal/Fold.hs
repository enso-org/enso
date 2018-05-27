-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Fold where

import Prologue hiding (Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered     as State
import qualified Data.Generics.Traversable       as GTraversable
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.Set   as Setx
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Foreign.Ptr                     as Ptr
import qualified Foreign.Storable                as Storable
import qualified Type.Data.List                  as List

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Data.Set                        (Set)
import Data.Vector.Storable.Foreign    (Vector)
import Foreign.Ptr.Utils               (SomePtr)
import Type.Data.Bool                  (Not, type (||))
-- import Data.PtrList.Mutable            (UnmanagedPtrList)



------------------
-- === Fold === --
------------------

-- === Definition === --

type family Result t

class Monad m => Builder t m a where
    build :: a -> m (Result t) -> m (Result t)

class Monad m => Builder1 t m a where
    build1 :: ∀ t1. a t1 -> m (Result t) -> m (Result t)


-- === Generics === --

gbuild :: ∀ t a m. (GTraversable (Builder t m) a, Applicative m)
      => a -> m (Result t) -> m (Result t)
gbuild = GTraversable.gfoldl' @(Builder t m) (\r d x -> r $! build @t d x) id
{-# INLINE gbuild #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} (GTraversable (Builder t m) a, Monad m)
      => Builder t m a where
    build = gbuild @t
    {-# INLINE build #-}


-- === No-op instances === --

-- FIXME: check if we really don't need them.
--        Luna/IR/Term.hs defaults to them (!)

-- instance {-# OVERLAPPABLE #-} Monad m => Builder s m (UnmanagedPtrList x) where
--     build = \_ -> id
--     {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} Monad m => Builder s m (Vector x) where
    build = \_ -> id
    {-# INLINE build #-}
