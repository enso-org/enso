{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE PolyKinds            #-}

module Data.RTuple where

import           Prelude hiding (map, init, last, length, take, concat, mapM, head, tail, reverse, drop, zip, zip3, unzip, unzip3)
import qualified Prelude as P

import Control.Lens.Utils     hiding (cons, uncons, reversed)
import Control.Monad.Identity hiding (mapM)
import Data.Default
import Data.List              (intercalate)
import Data.Typeable
import GHC.Prim               (Any, Constraint, unsafeCoerce#)
import GHC.TypeLits
import Type.Container         (Append, Concat, Index2, Reverse, type (<>))
import Type.List

-- === Definition === --

newtype RTuple (t :: [*]) = RTuple (Lst2RT t)

-- === Helpers === --

type family Lst2RT lst where
    Lst2RT '[]       = ()
    Lst2RT (a ': as) = (a, Lst2RT as)


-- === Instances === --

-- Show
deriving instance Show (Lst2RT t) => Show (RTuple t)

-- Wrappers
makeWrapped ''RTuple

-- Type concat
type instance Concat (RTuple a) (RTuple b) = RTuple (a <> b)


-- === Operations === --

---- Merge
--class Merge a b where merge :: a -> b -> a <> b
--instance {-# OVERLAPPABLE #-} r ~ Concat '[] r                                                                        => Merge (RTuple '[])       (RTuple r) where merge _                r = r
--instance {-# OVERLAPPABLE #-} (Lst2RT (Concat (l ': ls) r) ~ (l, Lst2RT (Concat ls r)), Merge (RTuple ls) (RTuple r)) => Merge (RTuple (l ': ls)) (RTuple r) where merge (RTuple (l, ls)) r = RTuple (l, fromRT $ merge (RTuple ls :: RTuple ls) r)


----


--class Append (a :: [*]) where append :: RTuple a -> RTuple b -> RTuple (a <> b)
--instance {-# OVERLAPPABLE #-}              Append '[]       where append _                r = r
--instance {-# OVERLAPPABLE #-} Append ls => Append (l ': ls) where append (RTuple (l, ls)) r = RTuple (l, unwrap' $ append (RTuple ls :: RTuple ls) r)



--append :: RTuple t -> RTuple t' -> RTuple (t <> t')
--append



-- === Definition === --

data List lst where
    Null :: List '[]
    Cons :: a -> List lst -> List (a ': lst)


-- === Type instance === --

type instance Concat (List a) (List b) = List (a <> b)


-- === Utils === --

-- Info

null :: List lst -> Bool
null Null = True
null _    = False
{-# INLINE null #-}

single :: List lst -> Bool
single (Cons _ Null) = True
single _             = False
{-# INLINE single #-}

length :: List lst -> Int
length Null         = 0
length (Cons _ lst) = succ $ length lst
{-# INLINE length #-}


-- Construction

empty :: List '[]
empty = Null
{-# INLINE empty #-}

singleton :: a -> List '[a]
singleton a = Cons a Null
{-# INLINE singleton #-}

fromList :: KnownNat len => Proxy (len :: Nat) -> [a] -> Maybe (List (Replicate len a))
fromList len lst = unsafeCoerce# . fromList' <$> safeTake (fromIntegral $ natVal len) lst where

    safeTake :: Int -> [a] -> Maybe [a]
    safeTake 0 lst    = Just []
    safeTake n []     = Nothing
    safeTake n (a:as) = (a :) <$> safeTake (pred n) as
    {-# INLINE safeTake #-}

    fromList' :: [a] -> Any
    fromList' []       = unsafeCoerce# Null
    fromList' (a : as) = unsafeCoerce# $ Cons a (unsafeCoerce# $ fromList' as)
    {-# INLINE fromList' #-}

{-# INLINE fromList #-}


-- Modification

concat :: List xs -> List ys -> List (xs <> ys)
concat Null        ys = ys
concat (Cons x xs) ys = Cons x $ concat xs ys
{-# INLINE concat #-}

append :: a -> List lst -> List (Append a lst)
append a Null        = Cons a Null
append a (Cons l ls) = Cons l $ append a ls
{-# INLINE append #-}

prepend :: a -> List lst -> List (a ': lst)
prepend = cons
{-# INLINE prepend #-}

-- insert


-- Transformations --

cons :: a -> List lst -> List (a ': lst)
cons = Cons
{-# INLINE cons #-}

uncons :: List (x ': xs) -> (x, List xs)
uncons = view unconsed
{-# INLINE uncons #-}

unconsed  :: Iso  (List (x ': xs)) (List (y ': ys)) (x, List xs) (y, List ys)
unconsed' :: Iso' (List (x ': xs))                  (x, List xs)
unconsed  = iso (\(Cons x xs) -> (x,xs)) (\(x,xs) -> Cons x xs)
unconsed' = unconsed
{-# INLINE unconsed  #-}
{-# INLINE unconsed' #-}

head  :: Lens  (List (x ': ls)) (List (y ': ls)) x y
head' :: Lens' (List (x ': ls))                  x
head  = unconsed . _1
head' = head
{-# INLINE head  #-}
{-# INLINE head' #-}

tail  :: Lens  (List (a ': xs)) (List (a ': ys)) (List xs) (List ys)
tail' :: Lens' (List (x ': xs))                  (List xs)
tail  = unconsed . _2
tail' = tail
{-# INLINE tail  #-}
{-# INLINE tail' #-}

init  :: Lens  (List lst) (List (DropInit lst <> ys)) (List (Init lst)) (List ys)
init' :: Lens' (List lst)                             (List (Init lst))
init  = lens takeInit (\lst ninint -> concat (dropInit lst) ninint)
init' = lens takeInit (\lst ninint -> unsafeCoerce# $ concat (dropInit lst) ninint)
{-# INLINE init  #-}
{-# INLINE init' #-}

takeInit :: List lst -> List (Init lst)
takeInit Null          = Null
takeInit (Cons x Null) = Null
takeInit (Cons x (Cons y ys)) = Cons x (takeInit $ Cons y ys)
{-# INLINE takeInit #-}

dropInit :: List lst -> List (DropInit lst)
dropInit Null                 = Null
dropInit (Cons x Null)        = Cons x Null
dropInit (Cons x (Cons y ys)) = dropInit $ Cons y ys
{-# INLINE dropInit #-}

last  :: Lens  (List lst) (List (Append x (Init lst))) (Last lst) x
last' :: Lens' (List lst)                              (Last lst)
last  = lens takeLast (\lst nlast ->                 append nlast (takeInit lst))
last' = lens takeLast (\lst nlast -> unsafeCoerce# $ append nlast (takeInit lst))
{-# INLINE last  #-}
{-# INLINE last' #-}

takeLast :: List lst -> Last lst
takeLast (Cons x Null)        = x
takeLast (Cons x (Cons y ys)) = takeLast $ Cons y ys
{-# INLINE takeLast #-}

reversed  :: Iso  (List a) (List (Reverse b)) (List (Reverse a)) (List b)
reversed' :: Iso' (List a)                    (List (Reverse a))
reversed  = iso reverse reverse
reversed' = iso reverse (unsafeCoerce# reverse)
{-# INLINE reversed  #-}
{-# INLINE reversed' #-}

reverse :: List lst -> List (Reverse lst)
reverse lst = reverse' lst empty where
    reverse' :: List lst -> List lst' -> List (Reverse' lst lst')
    reverse' Null        lst = lst
    reverse' (Cons l ls) lst = reverse' ls (Cons l lst)
    {-# INLINE reverse' #-}
{-# INLINE reverse #-}



-- Sublists --

take :: KnownNat n => Proxy (n :: Nat) -> List lst -> List (Take n lst)
take = unsafeCoerce# . take' . fromIntegral . natVal where
    take' :: Int -> List lst -> Any
    take' 0 _           = unsafeCoerce# Null
    take' n (Cons l ls) = unsafeCoerce# $ Cons l (unsafeCoerce# $ take' (pred n) ls)
    {-# INLINE take' #-}
{-# INLINE take #-}

drop :: KnownNat n => Proxy (n :: Nat) -> List lst -> List (Drop n lst)
drop = unsafeCoerce# . drop' . fromIntegral . natVal where
    drop' :: Int -> List lst -> Any
    drop' 0 lst         = unsafeCoerce# lst
    drop' n (Cons _ ls) = drop' (pred n) ls
    {-# INLINE drop' #-}
{-# INLINE drop #-}

taken  :: KnownNat n => Proxy (n :: Nat) -> Lens  (List lst) (List (Concat x (Drop n lst))) (List (Take n lst)) (List x)
taken' :: KnownNat n => Proxy (n :: Nat) -> Lens' (List lst)                                (List (Take n lst))
taken  n = lens (take n) (\lst ninit ->                 concat ninit (drop n lst))
taken' n = lens (take n) (\lst ninit -> unsafeCoerce# $ concat ninit (drop n lst))
{-# INLINE taken  #-}
{-# INLINE taken' #-}

dropped  :: KnownNat n => Proxy (n :: Nat) -> Lens  (List lst) (List (Concat (Take n lst) x)) (List (Drop n lst)) (List x)
dropped' :: KnownNat n => Proxy (n :: Nat) -> Lens' (List lst)                                (List (Drop n lst))
dropped  n = lens (drop n) (\lst ntail ->                 concat (take n lst) ntail)
dropped' n = lens (drop n) (\lst ntail -> unsafeCoerce# $ concat (take n lst) ntail)
{-# INLINE dropped  #-}
{-# INLINE dropped' #-}

--split' :: Int -> List lst -> (Any, Any)
--split'

--splitAt :: Int -> [a] -> ([a], [a])
-- group :: Eq a => [a] -> [[a]] Source

-- inits :: [a] -> [[a]]
-- tails :: [a] -> [[a]]


-- === Indexing === --

index :: KnownNat n => Proxy (n :: Nat) -> List lst -> Index2 n lst
index = unsafeCoerce# . index' . fromIntegral . natVal where
    index' :: Int -> List lst -> Any
    index' 0 (Cons a _) = unsafeCoerce# a
    index' i (Cons _ l) = index' (pred i) l
    {-# INLINE index' #-}
{-# INLINE index #-}


-- === Zipping === --

zip :: List lst -> List lst' -> List (Zip lst lst')
zip = zip2
{-# INLINE zip #-}

zip2 :: List lst1 -> List lst2 -> List (Zip2 lst1 lst2)
zip2 Null          _             = Null
zip2 _             Null          = Null
zip2 (Cons x1 xs1) (Cons x2 xs2) = Cons (x1,x2) $ zip2 xs1 xs2
{-# INLINE zip2 #-}

zip3 :: List lst1 -> List lst2 -> List lst3 -> List (Zip3 lst1 lst2 lst3)
zip3 Null          _             _             = Null
zip3 _             Null          _             = Null
zip3 _             _             Null          = Null
zip3 (Cons x1 xs1) (Cons x2 xs2) (Cons x3 xs3) = Cons (x1,x2,x3) $ zip3 xs1 xs2 xs3
{-# INLINE zip3 #-}

zip4 :: List lst1 -> List lst2 -> List lst3 -> List lst4 -> List (Zip4 lst1 lst2 lst3 lst4)
zip4 Null          _             _             _             = Null
zip4 _             Null          _             _             = Null
zip4 _             _             Null          _             = Null
zip4 _             _             _             Null          = Null
zip4 (Cons x1 xs1) (Cons x2 xs2) (Cons x3 xs3) (Cons x4 xs4) = Cons (x1,x2,x3,x4) $ zip4 xs1 xs2 xs3 xs4
{-# INLINE zip4 #-}

zip5 :: List lst1 -> List lst2 -> List lst3 -> List lst4 -> List lst5 -> List (Zip5 lst1 lst2 lst3 lst4 lst5)
zip5 Null          _             _             _             _             = Null
zip5 _             Null          _             _             _             = Null
zip5 _             _             Null          _             _             = Null
zip5 _             _             _             Null          _             = Null
zip5 _             _             _             _             Null          = Null
zip5 (Cons x1 xs1) (Cons x2 xs2) (Cons x3 xs3) (Cons x4 xs4) (Cons x5 xs5) = Cons (x1,x2,x3,x4,x5) $ zip5 xs1 xs2 xs3 xs4 xs5
{-# INLINE zip5 #-}


-- TODO[WD]: Unzipping is not straight-forward because we have to provide Haskell's constraint that the elements are tuples!

--type family   ListTuple (t :: k)
--type instance ListTuple '(l1, l2)             = (List l1, List l2)
--type instance ListTuple '(l1, l2, l3)         = (List l1, List l2, List l3)
--type instance ListTuple '(l1, l2, l3, l4)     = (List l1, List l2, List l3, List l4)
--type instance ListTuple '(l1, l2, l3, l4, l5) = (List l1, List l2, List l3, List l4, List l5)

--unzip2 :: List lst -> ListTuple (Unzip2 lst)
--unzip2 Null = (Null, Null)
--unzip2 (Cons (x1,x2) xs) = let (xs1, xs2) = unzip2 xs
--                           in (Cons x1 xs1, Cons x2 xs2)


-- === Instances === --

-- Show
instance Map Show lst => Show (List lst) where
    show lst = '[' : intercalate "," (map (Proxy :: Proxy Show) show lst) ++ "]"

-- Default
instance Default (List '[])                                         where def = Null         ; {-# INLINE def #-}
instance (Default l, Default (List ls)) => Default (List (l ': ls)) where def = Cons def def ; {-# INLINE def #-}





-- === Mapping === --

class    ctx a => CtxM ctx a (m :: * -> *)
instance ctx a => CtxM ctx a m

type  Map  ctx lst = MapM (CtxM ctx) lst Identity
class MapM ctx lst m where
    mapM :: Proxy ctx -> (forall a. ctx a m => a -> m b) -> List lst -> m [b]

map :: forall ctx lst a b. Map ctx lst => Proxy ctx -> (forall a. ctx a => a -> b) -> List lst -> [b]
map _ f lst = runIdentity $ mapM (Proxy :: Proxy (CtxM ctx)) (return . f) lst
{-# INLINE map #-}

instance Monad m                           => MapM ctx '[]       m where mapM _ _ _             = return []                     ; {-# INLINE mapM #-}
instance (Monad m, ctx l m, MapM ctx ls m) => MapM ctx (l ': ls) m where mapM ctx f (Cons l ls) = (:) <$> f l <*> mapM ctx f ls ; {-# INLINE mapM #-}
