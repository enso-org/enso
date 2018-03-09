{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeInType #-}

module Luna.IR.Test where

import Data.Coerce
import Data.Kind
import Prelude


----------------------------------------
-- === GADTs based implementation === --
----------------------------------------

newtype El a = El Int

data Lst (els :: [Type]) where
    Nil  :: Lst '[]
    Cons :: {-# UNPACK #-} !(El a) -> !(Lst els) -> Lst (a ': els)


empty :: Lst '[]
empty = Nil ; {-# INLINE empty #-}


typeTag :: forall a. Int -> El a
typeTag = coerce ; {-# INLINE typeTag #-}

cons :: forall a els. Int -> Lst els -> Lst (a ': els)
cons i lst = Cons (typeTag @a i) lst ; {-# INLINE cons #-}

class Get a els where get :: Lst els -> Int
instance {-# OVERLAPPABLE #-} Get a els
      => Get a (b ': els) where get (Cons _ l) = get @a l ; {-# INLINE get #-}
instance Get a (a ': els) where get (Cons i _) = coerce i ; {-# INLINE get #-}

class Set a els where set :: Int -> Lst els -> Lst els
instance {-# OVERLAPPABLE #-} Set a els
      => Set a (b ': els) where set v (Cons x l) = Cons x (set @a v l) ; {-# INLINE set #-}
instance Set a (a ': els) where set v (Cons i l) = Cons (coerce v) l   ; {-# INLINE set #-}


-- === Show === --

getInts :: Lst els -> [Int]
getInts = \case
    Nil -> []
    Cons i l -> coerce i : getInts l

instance Show (Lst els) where
    show = show . getInts




------------------------------------------------
-- === Type families based implementation === --
------------------------------------------------

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !T
       | Z


newtype Lst2 (els :: [Type]) = Lst2 T

toLst2 :: T -> Lst2 els
toLst2 = coerce ; {-# INLINE toLst2 #-}

fromLst2 :: Lst2 els -> T
fromLst2 = coerce ; {-# INLINE fromLst2 #-}

--
-- empty2 :: Lst2 '[]
-- empty2 = toLst2 Z ; {-# INLINE empty2 #-}
--
-- cons :: forall a. Int -> Lst2 els -> Lst2 (a ': els)
-- cons i lst = T



-------------------
-- === Tests === --
-------------------

data X
data Y
data Z

myLst :: Lst '[X,Y,Z]
myLst = cons @X 1
      $ cons @Y 2
      $ cons @Z 3
      $ empty
{-# INLINE myLst #-}

pureLoop_X :: Int -> IO ()
pureLoop_X i = do
    let go :: Lst '[X,Y,Z] -> Int -> IO ()
        go !l 0 = print l
        go !l j = do
            let !x' = get @X l + 1
                !l' = set @X x' l
            go l' $! j - 1
    go myLst i
    return ()

pureLoop_Z :: Int -> IO ()
pureLoop_Z i = do
    let go :: Lst '[X,Y,Z] -> Int -> IO ()
        go !l 0 = print l
        go !l j = do
            let !x' = get @Z l + 1
                !l' = set @Z x' l
            go l' $! j - 1
    go myLst i
    return ()


pureLoop_X2 :: Int -> IO ()
pureLoop_X2 i = do
    let go :: (Int, Int, Int) -> Int -> IO ()
        go !l 0 = print l
        go !(!x,!y,!z) j = do
            let !x' = x + 1
            go (x',y,z) $! j - 1
    go (1,2,3) i
    return ()

pureLoop_Z2 :: Int -> IO ()
pureLoop_Z2 i = do
    let go :: (Int, Int, Int) -> Int -> IO ()
        go !l 0 = print l
        go !(!x,!y,!z) j = do
            let !x' = z + 1
            go (x,y,x') $! j - 1
    go (1,2,3) i
    return ()
