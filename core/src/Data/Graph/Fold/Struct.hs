{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Struct where

import Prologue hiding (Traversable, fold, fold1, traverse)

import qualified Data.Generics.Traversable   as GTraversable
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.Graph.Fold.Class       as Fold

import Data.Generics.Traversable    (GTraversable)
import Data.Vector.Storable.Foreign (Vector)


-------------------------
-- === Struct Fold === --
-------------------------

-- === Definition === --

data Struct t
type instance Fold.Result (Struct t) = Fold.Result t


-- === Instances === --

-- instance {-# OVERLAPPABLE #-} Monad m => Fold.Builder (Struct t) m (Vector a) where
--     build = \_ -> id
--     {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} (GTraversable (Fold.Builder t m) a, Monad m)
      => Fold.Builder (Struct t) m a where
    build = Fold.gbuild @t
    {-# INLINE build #-}

instance Fold.Builder t m a
      => Fold.Builder1 (Struct t) m (Layer.Simple a) where
    build1 = Fold.build @t . unwrap
    {-# INLINE build1 #-}

instance Fold.Builder t m a
      => Fold.Builder (Struct t) m (Maybe a) where
    build = maybe id (Fold.build @t)
    {-# INLINE build #-}

