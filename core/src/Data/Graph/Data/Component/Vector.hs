{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Vector where

import Data.Graph.Data.Component.Class
import Prologue                        hiding (fromList)

import qualified Data.Construction            as Data
import qualified Data.Property                as Property
import qualified Data.Vector.Storable.Foreign as Vector
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1.Deriving   as Storable1

import Data.Vector.Storable.Foreign (Vector)
import Foreign.DynamicStorable      (DynamicStorable)
import Foreign.Storable             (Storable)


-----------------
-- === Vector === --
-----------------

-- === Definition === --

newtype ComponentVector comp layout
    = ComponentVector (Vector (Component comp layout))
    deriving (Eq, Show, Storable, DynamicStorable)
makeLenses       ''ComponentVector
Storable1.derive ''ComponentVector


-- === API === --

fromList :: MonadIO m
         => [Component comp layout] -> m (ComponentVector comp layout)
fromList = \lst -> wrap <$> Vector.fromList lst ; {-# INLINE fromList #-}

toList :: MonadIO m => ComponentVector comp layout -> m [Component comp layout]
toList = Vector.toList . unwrap ; {-# INLINE toList #-}


-- === Instances === --

type instance Property.Get Storable.Dynamics (ComponentVector _)
   = Storable.Dynamic

instance MonadIO m => Data.ShallowDestructor2 m ComponentVector where
    destructShallow2 = Data.destructShallow1 . unwrap
    {-# INLINE destructShallow2 #-}
