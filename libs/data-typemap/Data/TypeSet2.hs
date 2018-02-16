{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeSet2 where

import Prologue

import Type.Data.Ord
import qualified Type.Data.Set as Type


data T = T

---------------------
-- === TypeSet === --
---------------------

-- === Definition === --

newtype TypeSet (ks :: [Type]) = TypeSet { fromTypeSet :: TypeSetData ks }

type family TypeSetData lst where
    TypeSetData '[]       = ()
    TypeSetData (a ': as) = (a, TypeSetData as)

makeLenses ''TypeSet

untt :: TypeSet ks -> TypeSetData ks
untt = coerce ; {-# INLINE untt #-}


-- === Construction === --

instance ks ~ '[] => Mempty (TypeSet ks) where
    mempty = TypeSet () ; {-# INLINE mempty #-}


-- === Insert === --

class Insert a ks where
    insert :: a -> TypeSet ks -> TypeSet (Type.RawInsert a ks)

instance Insert k '[] where
    insert a (TypeSet s) = TypeSet (a, s) ; {-# INLINE insert #-}

instance Insert k (k ': as) where
    insert a (TypeSet (_,as)) = TypeSet (a, as) ; {-# INLINE insert #-}

instance {-# OVERLAPPABLE #-}
    ( b ~ (k < a), SubInsert__ b k a as
    , Type.RawInsert k (a ': as) ~ Type.RawSubInsert b k a as)
    => Insert k (a ': as) where insert = subInsert__ @b ; {-# INLINE insert #-}

class SubInsert__ (b :: Bool) k a as where
    subInsert__ :: k -> TypeSet (a ': as) -> TypeSet (Type.RawSubInsert b k a as)

instance SubInsert__ 'True k a as where
    subInsert__ k (TypeSet s) = TypeSet (k, s)
    {-# INLINE subInsert__ #-}

instance Insert k as => SubInsert__ 'False k a as where
    subInsert__ k (TypeSet (a,as)) = TypeSet (a, coerce $ insert k (TypeSet @as as))
    {-# INLINE subInsert__ #-}



-- === UnsafeLookup === --

class UnsafeLookup k ks where
    unsafeLookup :: TypeSet ks -> k

instance {-# OVERLAPPABLE #-} UnsafeLookup k ks
 => UnsafeLookup k (l ': ks) where
    unsafeLookup (untt -> !(_,!as)) = unsafeLookup @k (coerce as :: TypeSet ks) ; {-# INLINE unsafeLookup #-}

instance UnsafeLookup k (k ': ks) where
    unsafeLookup (untt -> !(!a,_)) = a ; {-# INLINE unsafeLookup #-}



-- === Show === --

class Show__ ks where show__ :: TypeSet ks -> [String]
instance Show__ '[] where show__ _ = []
instance (Show a, Show__ as) => Show__ (a ': as) where
    show__ (TypeSet (a, as)) = show a : show__ (TypeSet @as as)
instance Show__ ks => Show (TypeSet ks) where
    show s = "[" <> intercalate ", " (show__ s) <> "]"

--
-- type instance Cmp Int String = LT
-- type instance Cmp String Int = GT
--
--
--
-- test :: IO ()
-- test = do
--     let a = mempty :: TypeSet '[]
--         a2 = insert (5 :: Int) a
--         a3 = insert ("ala" :: String) a2
--         a4 = insert ("ala2" :: String) a3
--     print a2
--     print a3
--     print a4
--     print "test"
