{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Name.Multipart where

import Luna.Prelude hiding (toList)

import OCI.IR.Name.Class
import Data.Container.Mono
import Data.Foldable (toList)


---------------------------
-- === MultipartName === --
---------------------------

-- === Definition === --

data MultipartName = MultipartName { _base     :: Name
                                   , _segments :: [Name]
                                   , __hash    :: Name
                                   }
makeClassy ''MultipartName


-- === Construction === --

mkMultipartName :: Name -> [Name] -> MultipartName
mkMultipartName n segs = MultipartName n segs . concatNames $ intersperse "_" (n:segs)


-- === Instances === --

-- Hash
instance HashableName MultipartName where nameHash = multipartName_hash

-- Basics
instance Show MultipartName where show    = show . view nameHash
instance Eq   MultipartName where (==)    = (==)    `on` view nameHash
instance Ord  MultipartName where compare = compare `on` view nameHash

-- Conversions
instance Convertible MultipartName   Name            where convert = view nameHash
instance Convertible MultipartName   Text            where convert = convertVia @Name
instance Convertible MultipartName   String          where convert = convertVia @Name
instance Convertible Name            MultipartName   where convert = singleton
instance Convertible Text            MultipartName   where convert = convertVia @Name
instance Convertible String          MultipartName   where convert = convertVia @Name
instance Convertible (NonEmpty Name) MultipartName   where convert (n:|ns) = mkMultipartName n ns
instance Convertible MultipartName   (NonEmpty Name) where convert n = n ^. base :| n ^. segments

instance IsString MultipartName where fromString = convert

-- Appendable
type instance Item MultipartName = Name
instance Appendable MultipartName where
    appendMany segs m = m & segments %~ (<> toList segs)
                          & nameHash %~ (\h -> concatNames (h : "_" : toList segs))

-- Singleton
instance Singleton MultipartName where
    singleton n = MultipartName n [] n

-- Uniquable
instance Uniquable MultipartName where
    getUnique = getUnique . view nameHash
