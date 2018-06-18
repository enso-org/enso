{-# LANGUAGE UndecidableInstances #-}

module OCI.Data.Name.Qualified where

import Prologue

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified OCI.Data.Name.Class                as Name

import Foreign.Storable (Storable)

----------------------
-- === Qualified === --
----------------------

-- === Definition === --

newtype Qualified = Qualified Name.Name deriving (Eq, IsString, Ord, Show, Storable)
makeLenses          ''Qualified
GTraversable.derive ''Qualified

-- === Instances === --

instance Convertible Name.Name Qualified where
    convert = wrap
    {-# INLINE convert #-}

instance Convertible Qualified Name.Name where
    convert = unwrap
    {-# INLINE convert #-}

instance Convertible [Name.Name] Qualified where
    convert ns = convert $ Name.concat $ intersperse (convert ".") ns
    {-# INLINE convert #-}

type instance Item Qualified = Name.Name
