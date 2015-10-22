{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

module Data.List.Class where

import qualified GHC.Exts                as GHC
import qualified Data.Vector             as V

-- === Types ===

type family Item l

class FromList       l where fromList         :: [Item l] -> l
                             default fromList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                              => [Item l] -> l
                             fromList = GHC.fromList

class ToList         l where toList         :: l -> [Item l]
                             default toList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                            => l -> [Item l]
                             toList = GHC.toList

class FromListUnsafe l where fromListUnsafe :: [Item l] -> l
class ToListUnsafe   l where toListUnsafe   :: l -> [Item l]


class    (FromList l, ToList l) => IsList l
instance (FromList l, ToList l) => IsList l

class    (FromList l, ToList l) => IsListUnsafe l
instance (FromList l, ToList l) => IsListUnsafe l



-- === Instances ===

type instance Item (V.Vector a) = a
instance FromList  (V.Vector a)
instance ToList    (V.Vector a)
