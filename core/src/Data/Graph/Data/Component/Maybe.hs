{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Maybe where

import Prologue

import qualified Data.Generics.Traversable.Deriving  as GTraversable
import qualified Data.Graph.Fold.Class               as Fold
import qualified Data.Graph.Fold.Struct              as Struct
import qualified Foreign.Storable1.Deriving          as Storable1
import qualified Foreign.Storable.Deriving           as Storable

import Data.Graph.Data.Component.Class (Component)

newtype MaybeComponent tag layout
      = MaybeComponent (Maybe (Component tag layout))
      deriving Show
makeLenses ''MaybeComponent

instance (Monad m, Fold.Builder1 t m (Component tag))
      => Fold.Builder1 (Struct.Struct t) m (MaybeComponent tag) where
    build1 = maybe id (Fold.build1 @t) . unwrap
    {-# INLINE build1 #-}

instance Default1 (MaybeComponent tag) where
    def1 = MaybeComponent Nothing
    {-# INLINE def1 #-}

mapM :: Applicative a => (Component tag layout -> a (Component tag layout))
                      -> MaybeComponent tag layout -> a (MaybeComponent tag layout)
mapM f = fmap wrap . traverse f . unwrap
{-# INLINE mapM #-}

Storable.derive     ''MaybeComponent
Storable1.derive    ''MaybeComponent
GTraversable.derive ''MaybeComponent
