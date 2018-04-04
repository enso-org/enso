{-# LANGUAGE OverloadedStrings #-}

module OCI.Data.Name.Multipart where

import Prologue hiding (toList)

import qualified OCI.Data.Name.Class as Name

import Data.Foldable       (toList)
import OCI.Data.Name.Class (Name)


---------------------------
-- === Multipart === --
---------------------------

-- === Definition === --

data Multipart = Multipart
    { _base     :: !Name
    , _segments :: ![Name]
    , _hash     :: !Name
    }
makeLenses ''Multipart


-- === Construction === --

make :: Name -> [Name] -> Multipart
make n segs = Multipart n segs . Name.concat $ intersperse "_" (n:segs) ; {-# INLINE make #-}

singleton :: Name -> Multipart
singleton n = Multipart n mempty n ; {-# INLINE singleton #-}


-- === Instances === --

-- Hash
-- instance HashableName Multipart where nameHash = multipartName_hash

-- Basics
instance Show Multipart where show    = show . view hash       ; {-# INLINE show    #-}
instance Eq   Multipart where (==)    = (==)    `on` view hash ; {-# INLINE (==)    #-}
instance Ord  Multipart where compare = compare `on` view hash ; {-# INLINE compare #-}

-- Conversions
instance Convertible Multipart Name      where convert = view hash    ; {-# INLINE convert #-}
-- instance Convertible Multipart Text      where convert = convertVia @Name ; {-# INLINE convert #-}
instance Convertible Multipart String    where convert = convertVia @Name ; {-# INLINE convert #-}
instance Convertible Name      Multipart where convert = singleton        ; {-# INLINE convert #-}
-- instance Convertible Text      Multipart where convert = convertVia @Name ; {-# INLINE convert #-}
instance Convertible String    Multipart where convert = convertVia @Name ; {-# INLINE convert #-}
instance Convertible (NonEmpty Name) Multipart where
    convert (n:|ns) = make n ns ; {-# INLINE convert #-}
instance Convertible Multipart (NonEmpty Name) where
    convert n = n ^. base :| n ^. segments ; {-# INLINE convert #-}

instance IsString Multipart where fromString = convert ; {-# INLINE fromString #-}

-- -- Appendable
-- type instance Item Multipart = Name
-- instance Appendable Multipart where
--     appendMany segs m = m & segments %~ (<> toList segs)
--                           & nameHash %~ (\h -> concatNames (h : "_" : toList segs))

-- -- Singleton
-- instance Singleton Multipart where
--     singleton n = Multipart n [] n

-- -- Uniquable
-- instance Uniquable Multipart where
--     getUnique = getUnique . view nameHash
