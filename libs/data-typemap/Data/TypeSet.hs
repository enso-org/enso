{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeSet where

import Prologue

import Type.Data.Ord
import qualified Type.Data.Set as Type


data T = T !Int !T
       | Z


---------------------
-- === TypeSet === --
---------------------

-- === Definition === --

newtype TypeSet (ks :: [Type]) = TypeSet { fromTypeSet :: T }
makeLenses ''TypeSet

-- type family TypeSetData lst where
--     TypeSetData '[]       = Z
--     TypeSetData (a ': as) = T a (TypeSetData as)


untt :: TypeSet ks -> T
untt = coerce ; {-# INLINE untt #-}


-- === Construction === --

instance ks ~ '[] => Mempty (TypeSet ks) where
    mempty = TypeSet Z ; {-# INLINE mempty #-}


-- === Insert === --

class Insert a ks where
    insert :: a -> TypeSet ks -> TypeSet (Type.RawInsert a ks)

instance Coercible k Int => Insert k '[] where
    insert a (TypeSet s) = TypeSet (T (coerce a) s) ; {-# INLINE insert #-}

instance Coercible k Int => Insert k (k ': as) where
    insert a (TypeSet (T _ as)) = TypeSet (T (coerce a) as) ; {-# INLINE insert #-}

instance {-# OVERLAPPABLE #-}
    ( b ~ (k < a), SubInsert__ b k a as
    , Type.RawInsert k (a ': as) ~ Type.RawSubInsert b k a as)
    => Insert k (a ': as) where insert = subInsert__ @b ; {-# INLINE insert #-}

class SubInsert__ (b :: Bool) k a as where
    subInsert__ :: k -> TypeSet (a ': as) -> TypeSet (Type.RawSubInsert b k a as)

instance Coercible k Int => SubInsert__ 'True k a as where
    subInsert__ k (TypeSet s) = TypeSet (T (coerce k) s)
    {-# INLINE subInsert__ #-}

instance Insert k as => SubInsert__ 'False k a as where
    subInsert__ k (TypeSet (T a as)) = TypeSet (T a $ coerce $ insert k (TypeSet @as as))
    {-# INLINE subInsert__ #-}



-- === UnsafeLookup === --

class UnsafeLookup k ks where
    unsafeLookup :: TypeSet ks -> k

instance {-# OVERLAPPABLE #-} UnsafeLookup k ks
 => UnsafeLookup k (l ': ks) where
    unsafeLookup (untt -> (T _ as)) = unsafeLookup @k (coerce as :: TypeSet ks) ; {-# INLINE unsafeLookup #-}

instance Coercible k Int => UnsafeLookup k (k ': ks) where
    unsafeLookup (untt -> (T a _)) = coerce a ; {-# INLINE unsafeLookup #-}

--
--
-- -- === Show === --
--
-- class Show__ ks where show__ :: TypeSet ks -> [String]
-- instance Show__ '[] where show__ _ = []
-- instance (Show a, Show__ as) => Show__ (a ': as) where
--     show__ (TypeSet (a, as)) = show a : show__ (TypeSet @as as)
-- instance Show__ ks => Show (TypeSet ks) where
--     show s = "[" <> intercalate ", " (show__ s) <> "]"
--
-- --
-- -- type instance Cmp Int String = LT
-- -- type instance Cmp String Int = GT
-- --
-- --
-- --
-- -- test :: IO ()
-- -- test = do
-- --     let a = mempty :: TypeSet '[]
-- --         a2 = insert (5 :: Int) a
-- --         a3 = insert ("ala" :: String) a2
-- --         a4 = insert ("ala2" :: String) a3
-- --     print a2
-- --     print a3
-- --     print a4
-- --     print "test"
