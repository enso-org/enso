{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module OCI.IR.Name.Qualified (module OCI.IR.Name.Qualified, module X) where

import qualified Prelude as P
import Luna.Prelude hiding ((|>))

import OCI.IR.Name.Class as X
import Data.Container.Mono
import qualified Data.Foldable as Foldable
import Data.Binary (get, put)

import qualified OCI.IR.Name.Path as Name
import           OCI.IR.Name.Path (HasPath, path)
import qualified GHC.Exts as GHC
import qualified Data.List.NonEmpty as NonEmpty


----------------------
-- === QualName === --
----------------------

-- === Definition === --

data QualName = QualName { __path  :: Name.Path
                         , _target :: Name
                         , __hash  :: Name
                         } deriving (Generic)
makeClassy ''QualName


-- === Construction === --

mkQualName :: Name.Path -> Name -> QualName
mkQualName p n = QualName p n . concatNames $ phash : glue [n] where
    glue  = if phash == mempty then id else ("." :)
    phash = p ^. nameHash

-- | Function uncheckedQualNameFromPath is a little unsafe, because it returns mempty when
--   Path was empty, which could be unwanted bahaviour. Use with caution.
uncheckedQualNameFromPath :: Name.Path -> QualName
uncheckedQualNameFromPath p = uncurry (flip mkQualName) $ fmap Name.mkPath
                            $ splitHead (p ^. Name.segments) & _1 %~ fromMaybe mempty


-- === Instances === --

-- Attribs
instance HasPath      QualName where path     = qualName_path
instance HashableName QualName where nameHash = qualName_hash

-- Basics
instance Show    QualName where show    = show . view nameHash
instance Eq      QualName where (==)    = (==)    `on` view nameHash
instance Ord     QualName where compare = compare `on` view nameHash
instance Mempty  QualName where mempty  = QualName mempty mempty mempty
instance Default QualName where def     = mempty

-- Semigroup
instance Semigroup QualName where
    QualName p t _ <> QualName p' t' _ = mkQualName ((p |> t) <> p') t'


-- Binary
instance Binary QualName where
    put (QualName p n _) = put (p,n)
    get = P.uncurry mkQualName <$> get

-- Conversions
instance Convertible QualName        Name            where convert    = view nameHash
instance Convertible QualName        Text            where convert    = convertVia @Name
instance Convertible QualName        String          where convert    = convertVia @Name
instance Convertible Name            QualName        where convert    = singleton
instance Convertible Text            QualName        where convert    = convertVia @Name
instance Convertible String          QualName        where convert    = convertVia @Name
instance Convertible [Name]          QualName        where convert ls = mkQualName (convert $ init ls) (last ls)
instance Convertible QualName        [Name]          where convert ls = convert (ls ^. path) <> [ls ^. target]
instance Convertible QualName        (NonEmpty Name) where convert    = NonEmpty.fromList . convert
instance Convertible (NonEmpty Name) QualName        where convert    = convert . NonEmpty.toList
instance IsString QualName where fromString = convert

-- splitHead :: forall a. IsList a => a -> (Maybe (Item a), a)

-- Singleton
instance Singleton QualName where
    singleton n = QualName mempty n n

-- Uniquable
instance Uniquable QualName where
    getUnique = getUnique . view nameHash

-- List
type instance Item  QualName = Name
instance ToList     QualName where toList   = convert
instance FromList   QualName where fromList = convert
instance GHC.IsList QualName where
    type Item QualName = Item QualName
    fromList = fromList
    toList   = toList
