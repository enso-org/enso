{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Set
    (module Data.Graph.Data.Component.Set, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, ToList, fromList, toList)

import qualified Data.Construction          as Data
import qualified Data.Mutable.Class2        as Mutable
import qualified Data.Mutable.Plain         as Data
import qualified Data.Property              as Property
import qualified Foreign.Storable.Class     as Storable
import qualified Foreign.Storable1.Deriving as Storable1
import qualified Memory                     as Memory

import Data.Graph.Data.Component.Class (Component)
import Data.Mutable.Storable.SmallSet  (SmallSetA)
-- import Foreign.DynamicStorable (DynamicStorable)
import Foreign.Storable (Storable)



--------------------------
-- === ComponentSet === --
-------------------------------------------

-- === Definition === --

type    ComponentSet = ComponentSetA Memory.StdAllocator
newtype ComponentSetA alloc tag layout
      = ComponentSet (ComponentSet__ alloc tag layout)
    deriving (Show, Storable, Remove m, ToList m, Size m, Map m)

type ComponentSet__ alloc tag layout = SmallSetA alloc 0 (Component tag layout)


-- === Instances === --

deriving instance Insert m (ComponentSet__ alloc tag layout)
               => Insert m (ComponentSetA  alloc tag layout)

deriving instance Data.CopyInitializer m (ComponentSet__ alloc tag layout)
               => Data.CopyInitializer m (ComponentSetA  alloc tag layout)


deriving instance Mutable.Unswizzle m (ComponentSet__ alloc tag layout)
               => Mutable.Unswizzle m (ComponentSetA  alloc tag layout)

instance Data.CopyInitializer  m (ComponentSetA alloc tag ())
      => Data.CopyInitializer1 m (ComponentSetA alloc tag) where
    copyInitialize1 = \a -> Data.copyInitialize (coerce a :: ComponentSetA alloc tag ())
    {-# INLINE copyInitialize1 #-}

instance Mutable.Unswizzle  m (ComponentSetA alloc tag ())
      => Mutable.Unswizzle1 m (ComponentSetA alloc tag) where
    unswizzle1 = \a -> Mutable.unswizzle (coerce a :: ComponentSetA alloc tag ())
    {-# INLINE unswizzle1 #-}

-- type instance Property.Get Storable.Dynamics (ComponentSet _) = Storable.Dynamic

type instance Item (ComponentSetA _ tag layout) = Component tag layout

instance Storable.KnownConstantSize (ComponentSet__ alloc tag layout)
      => Storable.KnownConstantSize (ComponentSetA  alloc tag layout) where
    constantSize = Storable.constantSize @(ComponentSet__ alloc tag layout)
    {-# INLINE constantSize #-}

instance Storable.KnownSize t m (ComponentSet__ alloc tag layout)
      => Storable.KnownSize t m (ComponentSetA  alloc tag layout) where
    size = Storable.size @t . unwrap
    {-# INLINE size #-}

instance (FromList m (ComponentSet__ alloc comp layout), Functor m)
      =>  FromList m (ComponentSetA  alloc comp layout) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}

instance (New m (ComponentSet__ alloc comp layout), Functor m)
      =>  New m (ComponentSetA  alloc comp layout) where
    new = wrap <$> new
    {-# INLINE new #-}

-- instance ToList m (Unwrapped (ComponentSet comp layout))
--       => ToList m (ComponentSet comp layout) where
--     toList = toList . unwrap
--     {-# INLINE toList #-}

-- instance MonadIO m => Set.Set m (ComponentSet tag layout) where
--     new    = wrap <$> Set.new    ; {-# INLINE new    #-}
--     insert = Set.insert . unwrap ; {-# INLINE insert #-}
--     delete = Set.delete . unwrap ; {-# INLINE delete #-}
--     member = Set.member . unwrap ; {-# INLINE member #-}
--     size   = Set.size   . unwrap ; {-# INLINE size   #-}
--     null   = Set.null   . unwrap ; {-# INLINE null   #-}
--     toList = Set.toList . unwrap ; {-# INLINE toList #-}

-- instance MonadIO m => Data.Constructor2 m () ComponentSet where
--     construct2 = \ _ -> wrap <$> Data.construct1'
--     {-# INLINE construct2 #-}

instance MonadIO m => Data.ShallowDestructor2 m (ComponentSetA alloc) where
    destructShallow2 = Data.destruct1 . unwrap
    {-# INLINE destructShallow2 #-}

instance MonadIO m
      => Storable.KnownSize2 Storable.Dynamic m (ComponentSetA alloc) where
    size2 = Storable.size @Storable.Dynamic . unwrap
    {-# INLINE size2 #-}

makeLenses ''ComponentSetA
Storable1.derive ''ComponentSetA
