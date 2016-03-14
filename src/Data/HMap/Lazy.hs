{-# LANGUAGE   GADTs #-}
{-# LANGUAGE   DeriveDataTypeable #-}
{-# LANGUAGE   ScopedTypeVariables #-}
{-# LANGUAGE   ViewPatterns #-}
{-# LANGUAGE   FunctionalDependencies #-}
{-# LANGUAGE   StandaloneDeriving #-}
{-# LANGUAGE   TypeFamilies #-}
{-# LANGUAGE   UndecidableInstances #-}
{-# LANGUAGE   NoMonomorphismRestriction #-}
{-# LANGUAGE   DeriveFunctor #-}
{-# LANGUAGE   DeriveTraversable #-}

module Data.HMap.Lazy (
    module Data.HMap.Lazy,
    module X
) where

import Control.Lens
import Control.Lens.Utils
import Data.Functor.Utils

import           Data.Maps         as X
import           Prelude           hiding (lookup, (.))
import qualified Data.Maps         as Maps
import           Data.Monoid
import           Data.Typeable
import           Data.Default
import           Data.Traversable
import           Data.Map          (Map)
import           Data.IntMap       (IntMap)
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable     (Hashable)
import           Data.Foldable     (Foldable)
import           Type.Hidden       (Hidden)
import qualified Type.Hidden       as Type
import           GHC.Generics      (Generic)
import           Data.Container.Hetero

----------------------------------------------------------------------
-- Key
----------------------------------------------------------------------

data Key key val = Key key deriving Show
type IntKey = Key Int

class IsKey a k v | a -> k v where
    toKey :: a -> Key k v

-- == Instances ==

instance IsKey (Key k v) k v where
    toKey = id

instance Default k => Default (Key k v) where
    def = Key def

----------------------------------------------------------------------
-- Hetero wrapper
----------------------------------------------------------------------

-- == Utils ==

baseElems :: ValMap m k v => Hetero m -> [v]
baseElems = elems ∘ unwrap'

baseKeys :: ValMap m k v => Hetero m -> [k]
baseKeys = keys ∘ unwrap'

lookupBase :: GenMap m k v => k -> Hetero m -> Maybe v
lookupBase k = Maps.lookup k ∘ unwrap'

-- == Instances ==

instance (IsKey key k v, GenMap m k t, Hidden t) => GenMap (Hetero m) key v where
    lookup (toKey -> Key k) m = (fmap Type.reveal (Maps.lookup k $ unwrap' m) :: Maybe v)
    {-# INLINE lookup #-}

    insert (toKey -> Key k) a m = v `seq` (m & wrapped' %~ Maps.insert k v)
        where v   = Type.hide a
    {-# INLINE insert #-}

----------------------------------------------------------------------
-- Typeable sets
----------------------------------------------------------------------

type HHashMap k = Hetero (HashMap k Type.Any)
type HMap     k = Hetero (Map     k Type.Any)
type HIntMap    = Hetero (IntMap    Type.Any)

type HTHashMap = HHashMap TypeRep
type HTMap     = HMap     TypeRep

data TypeKey val = TypeKey deriving Show

instance Typeable v => IsKey (TypeKey v) TypeRep v where
    toKey (_ :: TypeKey val) = Key (typeOf (undefined :: val))

