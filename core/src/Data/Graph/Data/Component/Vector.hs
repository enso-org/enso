{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Vector
    (module Data.Graph.Data.Component.Vector, module X) where
import Data.Mutable.Class as X

import Data.Graph.Data.Component.Class
import Prologue                        hiding (FromList, ToList, fromList,
                                        toList)

import qualified Data.Construction            as Data
import qualified Data.Mutable.Class2          as Mutable
import qualified Data.Mutable.Plain           as Data
import qualified Foreign.Storable.Class       as Storable
import qualified Memory                       as Memory

import Data.Mutable.Storable.SmallAutoVector (UnmanagedSmallVectorA)
import Foreign.Storable                      (Storable)


-----------------
-- === Vector === --
-----------------

-- === Definition === --

type    ComponentVector = ComponentVectorA Memory.StdAllocator
newtype ComponentVectorA alloc comp layout
    = ComponentVector (ComponentVector__ alloc comp layout)
    deriving (Eq, Show, Storable, Map m) -- Storable, DynamicStorable)
-- Storable1.derive ''ComponentVector

type ComponentVector__ alloc comp layout
   = UnmanagedSmallVectorA alloc 0 (Component comp layout)

type instance Item (ComponentVectorA _ comp layout) = Component comp layout

makeLenses       ''ComponentVectorA


-- -- === API === --

-- fromList :: MonadIO m
--          => [Component comp layout] -> m (ComponentVector comp layout)
-- fromList = \lst -> wrap <$> Vector.fromList lst ; {-# INLINE fromList #-}

-- toList :: MonadIO m => ComponentVector comp layout -> m [Component comp layout]
-- toList = Vector.toList . unwrap ; {-# INLINE toList #-}


-- -- === Instances === --


deriving instance Data.CopyInitializer m (ComponentVector__ alloc tag layout)
               => Data.CopyInitializer m (ComponentVectorA  alloc tag layout)


deriving instance Mutable.Unswizzle m (ComponentVector__ alloc tag layout)
               => Mutable.Unswizzle m (ComponentVectorA  alloc tag layout)

instance Data.CopyInitializer  m (ComponentVectorA alloc tag ())
      => Data.CopyInitializer1 m (ComponentVectorA alloc tag) where
    copyInitialize1 = \a -> Data.copyInitialize (coerce a :: ComponentVectorA alloc tag ())
    {-# INLINE copyInitialize1 #-}

instance Mutable.Unswizzle  m (ComponentVectorA alloc tag ())
      => Mutable.Unswizzle1 m (ComponentVectorA alloc tag) where
    unswizzle1 = \a -> Mutable.unswizzle (coerce a :: ComponentVectorA alloc tag ())
    {-# INLINE unswizzle1 #-}

instance (FromList m (ComponentVector__ alloc comp layout), Functor m)
      => FromList m (ComponentVectorA alloc comp layout) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}

instance ToList m (ComponentVector__ alloc comp layout)
      => ToList m (ComponentVectorA alloc comp layout) where
    toList = toList . unwrap
    {-# INLINE toList #-}

-- type instance Property.Get Storable.Dynamics (ComponentVector _)
--    = Storable.Dynamic

instance MonadIO m => Data.ShallowDestructor2 m (ComponentVectorA alloc) where
    destructShallow2 = Data.destructShallow1 . unwrap
    {-# INLINE destructShallow2 #-}

instance MonadIO m
      => Storable.KnownSize2 Storable.Dynamic m (ComponentVectorA alloc) where
    size2 = Storable.size @Storable.Dynamic . unwrap
    {-# INLINE size2 #-}
