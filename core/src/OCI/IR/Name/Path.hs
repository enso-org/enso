{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module OCI.IR.Name.Path (module OCI.IR.Name.Path, module X) where

import Luna.Prelude

import OCI.IR.Name.Class as X
import Data.Container.Mono
import qualified Data.Foldable as Foldable
import Data.Binary (get, put)
import qualified GHC.Exts as GHC


------------------
-- === Path === --
------------------

-- === Definition === --

data Path = Path { _segments :: [Name]
                 , __hash    :: Name
                 }
makeClassy ''Path


-- === Construction === --

mkPath :: [Name] -> Path
mkPath segs = Path segs . concatNames $ intersperse "." segs


-- === Instances === --

-- Attribs
instance HashableName Path where nameHash = path_hash

-- Basics
instance Show Path where show    = show . view nameHash
instance Eq   Path where (==)    = (==)    `on` view nameHash
instance Ord  Path where compare = compare `on` view nameHash

-- Binary
instance Binary Path where
    put (Path s _) = put s
    get = mkPath <$> get

-- Monoid
instance Mempty    Path where mempty                = Path mempty mempty
instance Semigroup Path where Path s _ <> Path s' _ = mkPath (s <> s')

-- Conversions
instance {-# OVERLAPPABLE #-} Convertible' a Name
      => Convertible [a]    Path   where convert = mkPath . fmap convert'
instance Convertible Path   Name   where convert = view nameHash
instance Convertible Path   Text   where convert = convertVia @Name
instance Convertible Path   String where convert = convertVia @Name
instance Convertible Name   Path   where convert = singleton
instance Convertible Text   Path   where convert = convertVia @Name
instance Convertible String Path   where convert = convertVia @Name
instance Convertible Path   [Name] where convert = view segments
instance IsString Path where fromString = convert

-- List
type instance Item  Path = Name
instance FromList   Path where fromList = convert
instance ToList     Path where toList   = convert
instance GHC.IsList Path where
    type Item Path = Item Path
    fromList = fromList
    toList   = toList

-- Appendable
instance Appendable Path where
    appendMany segs m = m & segments %~ (<> Foldable.toList segs)
                          & nameHash %~ (\h -> concatNames (h : "." : Foldable.toList segs))

-- Singleton
instance Singleton Path where
    singleton n = Path [n] n

-- Uniquable
instance Uniquable Path where
    getUnique = getUnique . view nameHash
