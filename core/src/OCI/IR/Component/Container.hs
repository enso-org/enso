{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Container where

import OCI.IR.Component.Class
import Prologue

import qualified Data.Set.Mutable.Class     as Set
import qualified Foreign.Storable1.Deriving as Storable1

import Data.PtrSet.Mutable (UnmanagedPtrSet)
import Foreign.Storable    (Storable)


-----------------
-- === Set === --
-----------------

newtype Set comp layout = Set (UnmanagedPtrSet (Component comp layout))
    deriving (Show, Storable)
makeLenses       ''Set
Storable1.derive ''Set

type instance Set.Item (Set comp layout) = Component comp layout
instance MonadIO m => Set.Set m (Set comp layout) where
    new    = wrap <$> Set.new    ; {-# INLINE new    #-}
    insert = Set.insert . unwrap ; {-# INLINE insert #-}
    delete = Set.delete . unwrap ; {-# INLINE delete #-}
    member = Set.member . unwrap ; {-# INLINE member #-}
    size   = Set.size   . unwrap ; {-# INLINE size   #-}
    null   = Set.null   . unwrap ; {-# INLINE null   #-}
    toList = Set.toList . unwrap ; {-# INLINE toList #-}
