{-# LANGUAGE UndecidableInstances #-}

module OCI.Data.Name.Qualified where

import Prologue

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Foreign.Storable.Class             as Storable
import qualified OCI.Data.Name.Class                as Name

import Foreign.Storable (Storable)


----------------------
-- === Qualified === --
----------------------

-- === Definition === --

newtype Qualified = Qualified Name.Name
    deriving (Eq, IsString, Ord, Show, Storable)
makeLenses          ''Qualified
GTraversable.derive ''Qualified

instance MonadIO m => Storable.Peek t m Qualified
instance MonadIO m => Storable.Poke t m Qualified
instance Storable.KnownConstantSize Qualified where
    constantSize = Storable.constantSize @(Unwrapped Qualified)
    {-# INLINE constantSize #-}


-- === Instances === --

type instance Item Qualified = Name.Name

instance Convertible Name.Name Qualified where
    convert = wrap
    {-# INLINE convert #-}

instance Convertible Qualified Name.Name where
    convert = unwrap
    {-# INLINE convert #-}

instance Convertible [Name.Name] Qualified where
    convert = convert . Name.concat . intersperse (convert ".")
    {-# INLINE convert #-}

instance Convertible Qualified String where
    convert = convertVia @Name.Name
    {-# INLINE convert #-}
