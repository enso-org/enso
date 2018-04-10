{-# LANGUAGE OverloadedStrings #-}

module OCI.Data.Name.Multipart where

import Prologue hiding (toList)

import qualified Data.List.Utils        as List
import qualified Foreign.Storable.Utils as Storable
import qualified OCI.Data.Name.Class    as Name

import Data.Foldable          (toList)
import Foreign.Storable.Utils (Storable)
import OCI.Data.Name.Class    (Name)


---------------------------
-- === Multipart === --
---------------------------

-- === Definition === --

data Multipart = Multipart
    { _segments :: !(NonEmpty Name)
    , _hash     :: !Name
    }
makeLenses ''Multipart


-- === Construction === --

separator :: Char
separator = '\RS' ; {-# INLINE separator #-}

make :: NonEmpty Name -> Multipart
make segs = Multipart segs . Name.concat $ intersperse (convert separator) segs ; {-# INLINE make #-}

singleton :: Name -> Multipart
singleton n = Multipart (pure n) n ; {-# INLINE singleton #-}

fromHash :: Name -> Maybe Multipart
fromHash n = case convert <$> List.splitOn separator (convertTo @String n) of
    []     -> Nothing
    (a:as) -> Just . make $ a :| as



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
instance Convertible (NonEmpty Name) Multipart where convert   = make ; {-# INLINE convert #-}
instance Convertible Multipart (NonEmpty Name) where convert n = n ^. segments ; {-# INLINE convert #-}

instance IsString Multipart where fromString = convert ; {-# INLINE fromString #-}


-- | Warning! The Storable Name.Multipart instance is not efficient.
--   The `peek` function reads the Name ID (Int) from memory, lookups its real
--   FastString representation, converts it to string, divides it on special
--   chars and for each chunk lookups it back in the name map.
--   Use with care, only in non-performance related code.
instance Storable Multipart where
    sizeOf _ = Storable.sizeOf' @Name                                   ; {-# INLINE sizeOf #-}
    peek ptr = unsafeFromJust . fromHash <$> Storable.peek (coerce ptr) ; {-# INLINE peek   #-}
    poke ptr = Storable.poke (coerce ptr) . view hash                   ; {-# INLINE poke   #-}
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
