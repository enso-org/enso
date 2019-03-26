{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeSet3 where

import Prologue hiding (set)

import qualified Data.Tuple.Strict.IntTuple as Tuple
import qualified Type.Data.List             as List



------------------------
-- === IntTypeMap === --
------------------------

-- === Definition === --

type IntTypeMapData els = Tuple.FromNat (List.Length els)
newtype IntTypeMap (els :: [Nat]) = IntTypeMap (IntTypeMapData els)


-- === API === --

type IntTypeMapIndexable idx els =
    ( Tuple.IxElemGetter idx (IntTypeMapData els)
    , Tuple.IxElemSetter idx (IntTypeMapData els)
    )

getAt :: forall (idx :: Nat) els. IntTypeMapIndexable idx els => IntTypeMap els -> Int
setAt :: forall (idx :: Nat) els. IntTypeMapIndexable idx els => Int -> IntTypeMap els -> IntTypeMap els
getAt   (IntTypeMap tup) = Tuple.getElemAt @idx tup                ; {-# INLINE getAt #-}
setAt a (IntTypeMap tup) = IntTypeMap $ Tuple.setElemAt @idx a tup ; {-# INLINE setAt #-}



-- === Construction utils === --

class    ZeroTuple tup       where zeroTuple :: tup
instance ZeroTuple Tuple.T0  where zeroTuple = Tuple.T0  ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T1  where zeroTuple = Tuple.T1  0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T2  where zeroTuple = Tuple.T2  0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T3  where zeroTuple = Tuple.T3  0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T4  where zeroTuple = Tuple.T4  0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T5  where zeroTuple = Tuple.T5  0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T6  where zeroTuple = Tuple.T6  0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T7  where zeroTuple = Tuple.T7  0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T8  where zeroTuple = Tuple.T8  0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T9  where zeroTuple = Tuple.T9  0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T10 where zeroTuple = Tuple.T10 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T11 where zeroTuple = Tuple.T11 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T12 where zeroTuple = Tuple.T12 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T13 where zeroTuple = Tuple.T13 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T14 where zeroTuple = Tuple.T14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T15 where zeroTuple = Tuple.T15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T16 where zeroTuple = Tuple.T16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T17 where zeroTuple = Tuple.T17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T18 where zeroTuple = Tuple.T18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T19 where zeroTuple = Tuple.T19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}
instance ZeroTuple Tuple.T20 where zeroTuple = Tuple.T20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; {-# INLINE zeroTuple #-}


zeroIntTypeMap :: ZeroTuple (IntTypeMapData ts) => IntTypeMap ts
zeroIntTypeMap = IntTypeMap zeroTuple ; {-# INLINE zeroIntTypeMap #-}



-- === Instances === --

deriving instance Mempty (IntTypeMapData els) => Mempty (IntTypeMap els)
deriving instance Show   (IntTypeMapData els) => Show   (IntTypeMap els)







data X
data Y
data Z

test :: IO ()
test = do
    pure ()
    -- let lst = setAt @0 11
    --         $ zeroIntTypeMap :: IntTypeMap '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
    --
    -- print lst


myLst :: IntTypeMap '[1,2,3,4,5]
myLst = setAt @0 11
      $ zeroIntTypeMap
{-# INLINE myLst #-}

pureLoop_X :: Int -> IO ()
pureLoop_X i = do
    let go :: IntTypeMap '[1,2,3,4,5] -> Int -> IO ()
        go _  0 = pure ()
        go !l j = do
            let !x' = getAt @0 l + 1
                !l' = setAt @0 x' l
            go l' $! j - 1
    go myLst i
    pure ()

pureLoop_Z :: Int -> IO ()
pureLoop_Z i = do
    let go :: IntTypeMap '[1,2,3,4,5] -> Int -> IO ()
        go _  0 = pure ()
        go !l j = do
            let !x' = getAt @4 l + 1
                !l' = setAt @4 x' l
            go l' $! j - 1
    go myLst i
    pure ()



-- newtype IntTypeMap (els :: [Type]) = IntTypeMap (TupleFrom els)
--
-- type family TupleFrom els where
--     TupleFrom '[]   = ()
--     TupleFrom '[t1] = t1
--     TupleFrom '' = a
--
-- data T = T !Int !T
--        | Z
--
--
-- ---------------------
-- -- === IntTypeMap === --
-- ---------------------
--
-- -- === Definition === --
--
-- newtype IntTypeMap (ks :: [Type]) = IntTypeMap { fromIntTypeMap :: T }
-- makeLenses ''IntTypeMap
--
-- -- type family IntTypeMapData lst where
-- --     IntTypeMapData '[]       = Z
-- --     IntTypeMapData (a ': as) = T a (IntTypeMapData as)
--
--
-- untt :: IntTypeMap ks -> T
-- untt = coerce ; {-# INLINE untt #-}
--
--
-- -- === Construction === --
--
-- instance ks ~ '[] => Mempty (IntTypeMap ks) where
--     mempty = IntTypeMap Z ; {-# INLINE mempty #-}
--
--
-- -- === Insert === --
--
-- class Insert a ks where
--     insert :: a -> IntTypeMap ks -> IntTypeMap (Type.RawInsert a ks)
--
-- instance Coercible k Int => Insert k '[] where
--     insert a (IntTypeMap s) = IntTypeMap (T (coerce a) s) ; {-# INLINE insert #-}
--
-- instance Coercible k Int => Insert k (k ': as) where
--     insert a (IntTypeMap (T _ as)) = IntTypeMap (T (coerce a) as) ; {-# INLINE insert #-}
--
-- instance {-# OVERLAPPABLE #-}
--     ( b ~ (k < a), SubInsert__ b k a as
--     , Type.RawInsert k (a ': as) ~ Type.RawSubInsert b k a as)
--     => Insert k (a ': as) where insert = subInsert__ @b ; {-# INLINE insert #-}
--
-- class SubInsert__ (b :: Bool) k a as where
--     subInsert__ :: k -> IntTypeMap (a ': as) -> IntTypeMap (Type.RawSubInsert b k a as)
--
-- instance Coercible k Int => SubInsert__ 'True k a as where
--     subInsert__ k (IntTypeMap s) = IntTypeMap (T (coerce k) s)
--     {-# INLINE subInsert__ #-}
--
-- instance Insert k as => SubInsert__ 'False k a as where
--     subInsert__ k (IntTypeMap (T a as)) = IntTypeMap (T a $ coerce $ insert k (IntTypeMap @as as))
--     {-# INLINE subInsert__ #-}
--
--
--
-- -- === UnsafeLookup === --
--
-- class UnsafeLookup k ks where
--     unsafeLookup :: IntTypeMap ks -> k
--
-- instance {-# OVERLAPPABLE #-} UnsafeLookup k ks
--  => UnsafeLookup k (l ': ks) where
--     unsafeLookup (untt -> (T _ as)) = unsafeLookup @k (coerce as :: IntTypeMap ks) ; {-# INLINE unsafeLookup #-}
--
-- instance Coercible k Int => UnsafeLookup k (k ': ks) where
--     unsafeLookup (untt -> (T a _)) = coerce a ; {-# INLINE unsafeLookup #-}
--
-- --
-- --
-- -- -- === Show === --
-- --
-- -- class Show__ ks where show__ :: IntTypeMap ks -> [String]
-- -- instance Show__ '[] where show__ _ = []
-- -- instance (Show a, Show__ as) => Show__ (a ': as) where
-- --     show__ (IntTypeMap (a, as)) = show a : show__ (IntTypeMap @as as)
-- -- instance Show__ ks => Show (IntTypeMap ks) where
-- --     show s = "[" <> intercalate ", " (show__ s) <> "]"
-- --
-- -- --
-- -- -- type instance Cmp Int String = LT
-- -- -- type instance Cmp String Int = GT
-- -- --
-- -- --
-- -- --
-- -- -- test :: IO ()
-- -- -- test = do
-- -- --     let a = mempty :: IntTypeMap '[]
-- -- --         a2 = insert (5 :: Int) a
-- -- --         a3 = insert ("ala" :: String) a2
-- -- --         a4 = insert ("ala2" :: String) a3
-- -- --     print a2
-- -- --     print a3
-- -- --     print a4
-- -- --     print "test"

