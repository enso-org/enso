module Data.AssocList where

import qualified Prelude   as P
import           Prologue_old  hiding (null, lookup)
import qualified Data.List as List


-----------------------
-- === AssocList === --
-----------------------

-- === Definition === --

newtype AssocList k a = AssocList [(k,a)] deriving (Show, Ord, Eq, Generic, Functor, Foldable, Traversable, Mempty, Semigroup, NFData, P.Monoid)
makeLenses ''AssocList


-- === Construction === --

singleton :: k -> a -> AssocList k a
singleton k a = uncheckedPrepend k a mempty


-- === Props === --

null :: AssocList k a -> Bool
null = List.null . unwrap

size :: AssocList k a -> Int
size = List.length . unwrap

keys :: AssocList k a -> [k]
keys = fmap fst . unwrap

elems :: AssocList k a -> [a]
elems = fmap snd . unwrap

assocs :: AssocList k a -> [(k,a)]
assocs = unwrap


-- === Query === --

lookup :: Eq k => k -> AssocList k a -> Maybe a
lookup k = List.lookup k . unwrap

lookupWithDef :: Eq k => a -> k -> AssocList k a -> a
lookupWithDef a = fromMaybe a .: lookup

member :: Eq k => k -> AssocList k a -> Bool
member = isJust .: lookup

notMember :: Eq k => k -> AssocList k a -> Bool
notMember = not .: member


-- === Modification === --

uncheckedPrepend :: k -> a -> AssocList k a -> AssocList k a
uncheckedPrepend k a = wrapped %~ ((k,a):)

uncheckedAppend :: k -> a -> AssocList k a -> AssocList k a
uncheckedAppend k a = wrapped %~ (<> [(k,a)])

insert :: Eq k => k -> a -> AssocList k a -> AssocList k a
insert = insertWith const

insertWith :: Eq k => (a -> a -> a) -> k -> a -> AssocList k a -> AssocList k a
insertWith = insertWithKey . const

insertWithKey :: Eq k => (k -> a -> a -> a) -> k -> a -> AssocList k a -> AssocList k a
insertWithKey f k a = wrapped %~ go where
    go []             = [(k,a)]
    go ((k',a') : ls) = if k == k' then (k,f k a a') : ls else (k',a') : go ls


-- === Update === --

delete :: Eq k => k -> AssocList k a -> AssocList k a
delete k = wrapped %~ go where
    go []            = []
    go ((k',a) : ls) = if k == k' then ls else (k',a) : go ls

adjust :: Eq k => (a -> a) -> k -> AssocList k a -> AssocList k a
adjust f k = wrapped %~ go where
    go []             = []
    go ((k',a') : ls) = if k == k' then (k, f a') : ls else (k',a') : go ls


-- === MApping === --

mapWithKey :: (k -> a -> a) -> AssocList k a -> AssocList k a
mapWithKey f = wrapped %~ go where
    go []           = []
    go ((k,a) : ls) = (k, f k a) : go ls


-- === Instances === --

type instance Item (AssocList k a) = (k,a)
instance  ToList   (AssocList k a) where toList   = unwrap
instance  FromList (AssocList k a) where fromList = wrap
