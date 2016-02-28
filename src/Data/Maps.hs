---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Maps where

import Prelude hiding (lookup, foldr)
import qualified Data.Map     as Map
import           Data.Map     (Map)
import qualified Data.IntMap  as IntMap
import           Data.IntMap  (IntMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable     (Hashable)
import Data.Monoid
import Data.Foldable (Foldable, foldr)
import Data.Maybe (isJust)


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class GenMap m k a => ValPartMap m k a | m -> a

class ValPartMap m k a => ValMap m k a | m -> k a where
    elems :: m -> [a]
    keys  :: m -> [k]

    default keys :: m -> [k]
    keys = foldrWithKey (\k _ ks -> k : ks) []

class GenMap     m k a => KeyPartMap m k a | k -> a
class KeyPartMap m k a => KeyMap     m k a | k -> m a

class GenMap m k a | m k -> a where
    lookup          :: k -> m -> Maybe a
    insertWithKey   :: (k -> a -> a -> a) -> k -> a -> m -> m
    foldrWithKey    :: (k -> a -> b -> b) -> b -> m -> b
    delete          :: k -> m -> m
    size            :: m -> Int
    null            :: m -> Bool
    union           :: m -> m -> m

    -- optional
    insert          :: k -> a -> m -> m
    insertWith      :: (a -> a -> a) -> k -> a -> m -> m
    member          :: k -> m -> Bool
    notMember       :: k -> m -> Bool
    findWithDefault :: a -> k -> m -> a

    default insert :: k -> a -> m -> m
    insert = insertWith const

    default insertWith :: (a -> a -> a) -> k -> a -> m -> m
    insertWith f = insertWithKey (const f)

    default member :: k -> m -> Bool
    member k = isJust . lookup k

    default notMember :: k -> m -> Bool
    notMember k = not . member k

    default findWithDefault :: a -> k -> m -> a
    findWithDefault def k m = case lookup k m of
        Nothing -> def
        Just x  -> x

------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

-- Data.Map
instance Ord k => ValPartMap (Map k a) k a
instance Ord k => ValMap (Map k a) k a where
    elems           = Map.elems
    keys            = Map.keys

instance (Ord k, k~k', a~a') => GenMap (Map k a) k' a' where
    null            = Map.null
    size            = Map.size
    member          = Map.member
    notMember       = Map.notMember
    lookup          = Map.lookup
    findWithDefault = Map.findWithDefault

    insert          = Map.insert
    insertWith      = Map.insertWith
    insertWithKey   = Map.insertWithKey
    foldrWithKey    = Map.foldrWithKey
    delete          = Map.delete
    union           = Map.union

-- Data.IntMap
instance ValPartMap (IntMap a) Int a
instance ValMap (IntMap a) Int a where
    elems           = IntMap.elems
    keys            = IntMap.keys

instance (a~a') => GenMap (IntMap a) Int a' where
    null            = IntMap.null
    size            = IntMap.size
    member          = IntMap.member
    notMember       = IntMap.notMember
    lookup          = IntMap.lookup
    findWithDefault = IntMap.findWithDefault

    insert          = IntMap.insert
    insertWith      = IntMap.insertWith
    insertWithKey   = IntMap.insertWithKey
    foldrWithKey    = IntMap.foldrWithKey
    delete          = IntMap.delete
    union           = IntMap.union

-- Data.HashMap
instance (Hashable k, Eq k) => ValPartMap (HashMap k a) k a
instance (Hashable k, Eq k) => ValMap (HashMap k a) k a where
    elems           = HashMap.elems
    keys            = HashMap.keys

instance (Hashable k, Eq k, k~k', a~a') => GenMap (HashMap k a) k' a' where
    null            = HashMap.null
    size            = HashMap.size
    member          = HashMap.member
    lookup          = HashMap.lookup

    insert          = HashMap.insert
    insertWith      = HashMap.insertWith
    insertWithKey   = error "FIXME: Data.Maps does not define insertWithKey for HashMap"
    foldrWithKey    = HashMap.foldrWithKey
    delete          = HashMap.delete
    union           = HashMap.union
