{-# LANGUAGE UndecidableInstances #-}

module Data.Container.List where

import Data.Container.Class

import Prelude

import           Control.Lens
import qualified GHC.Exts       as GHC
import qualified Data.Vector    as V
import qualified Data.Text      as Text
import           Data.Text      (Text)
import qualified Data.Text.Lazy as LazyText
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Map       (Map)
import qualified Data.Map       as Map

type LazyText = LazyText.Text


class FromList       l where fromList         :: [Item l] -> l
                             default fromList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                              => [Item l] -> l
                             fromList = GHC.fromList

class ToList         l where toList         :: l -> [Item l]
                             default toList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                            => l -> [Item l]
                             toList = GHC.toList


class ToSet a where
    toSet :: a -> Set (Item a)
    default toSet :: (ToList a, Ord (Item a)) => a -> Set (Item a)
    toSet = Set.fromList . toList ; {-# INLINE toSet #-}


type IsList l = (FromList l, ToList l)

asList :: IsList a => Iso' a [Item a]
asList = iso toList fromList


-- === Instances ===

type instance Item Text = Char
instance ToList    Text where toList   = Text.unpack
instance FromList  Text where fromList = Text.pack


type instance Item LazyText = Char
instance ToList    LazyText where toList   = LazyText.unpack
instance FromList  LazyText where fromList = LazyText.pack



type instance Item (Set a) = a
instance Ord a => FromList (Set a) where fromList = Set.fromList ; {-# INLINE fromList #-}

type instance Item (Map k a) = (k ,a)
instance Ord k => FromList (Map k a) where fromList = Map.fromList ; {-# INLINE fromList #-}



instance {-# OVERLAPPABLE #-} (ToList a, Ord (Item a)) => ToSet a
instance                                                  ToSet (Set a) where toSet = id ; {-# INLINE toSet #-}
