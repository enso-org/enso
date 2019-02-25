{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.List (module Type.Data.List, module X) where

import Prelude

import Type.Data.Semigroup as X (type (<>))

import Data.Kind
import GHC.TypeLits
import Type.Data.Maybe



-- === Info === --

type family Null lst where
    Null '[] = 'True
    Null _   = 'False

type family Length lst where
    Length '[]       = 0
    Length (_ ': ls) = 1 + Length ls


-- === Construciton === --

type Cons a lst = a ': lst

type family Snoc (a :: k) (lst :: [k]) :: [k] where
    Snoc a '[]       = '[a]
    Snoc a (l ': ls) = l ': Snoc a ls

type family Uncons (lst :: [k]) :: Maybe (k, [k]) where
    Uncons '[] = 'Nothing
    Uncons (a ': as) = 'Just '(a,as)

type instance (a :: [k]) <> (b :: [k]) = Append a b
type family Append (lst :: [k]) (lst' :: [k]) :: [k] where
    Append '[]       lst = lst
    Append (l ': ls) lst = l ': Append ls lst

type family ConsAll (el :: k) (lst :: [[k]]) :: [[k]] where
    ConsAll _ '[] = '[]
    ConsAll a (l ': ls) = (a ': l) ': ConsAll a ls


-- === Basic operations === --

type Head' lst = FromJust (Head lst)
type family Head (lst :: [k]) :: Maybe k where
    Head '[]      = 'Nothing
    Head (a ': _) = 'Just a

type Tail' lst = FromJust (Tail lst)
type family Tail (lst :: [k]) :: Maybe [k] where
    Tail '[]       = 'Nothing
    Tail (_ ': as) = 'Just as

type Last' lst = FromJust (Last lst)
type family Last (lst :: [k]) :: Maybe k where
    Last '[a]      = 'Just a
    Last (a ': as) = Last as
    Last '[]       = 'Nothing

type Init' lst = FromJust (Init lst)
type family Init (lst :: [k]) :: Maybe [k] where
    Init '[]       = 'Nothing
    Init '[a]      = 'Just '[]
    Init (a ': as) = 'Just (a ': FromJust (Init as))

type family DropInit (lst :: [k]) :: [k] where
    DropInit '[]       = '[]
    DropInit '[a]      = '[a]
    DropInit (a ': as) = DropInit as


-- === Transformations === --

type family Map (f :: a -> b) (lst :: [a]) :: [b] where
    Map f '[]       = '[]
    Map f (a ': as) = f a ': Map f as

type Reverse lst = Reverse__ lst '[]
type family Reverse__ lst lst' where
    Reverse__ '[]       lst = lst
    Reverse__ (l ': ls) lst = Reverse__ ls (l ': lst)


-- === Sublists === --

type family Take n lst where
    Take 0 lst       = '[]
    Take n (l ': ls) = l ': Take (n - 1) ls

type family Drop n lst where
    Drop 0 lst       = lst
    Drop n (l ': ls) = Drop (n - 1) ls

type family TakeUntil (a :: k) (ls :: [k]) :: [k] where
    TakeUntil a '[]       = '[]
    TakeUntil a (a ': ls) = '[a]
    TakeUntil a (l ': ls) = l ': TakeUntil a ls


-- === Indexing === --

type Index' n lst = FromJust (Index n lst)
type family Index (n :: Nat) (lst :: [k]) :: Maybe k where
    Index 0 (a ': _ ) = 'Just a
    Index n (a ': as) = Index (n - 1) as
    Index _ '[]       = 'Nothing

type ElemIndex' a lst = FromJust (ElemIndex a lst)
type family ElemIndex (a :: k) (lst :: [k]) :: Maybe Nat where
    ElemIndex a (a ': ls) = 'Just 0
    ElemIndex a (_ ': ls) = SuccMaybe (ElemIndex a ls)
    ElemIndex _ '[]       = 'Nothing

type family In (a :: k) (lst :: [k]) :: Bool where
    In _ '[]       = 'False
    In a (a ': as) = 'True
    In a (_ ': as) = In a as


-- === Modification === --

type family Update (n :: Nat) (a :: k) (lst :: [k]) :: [k] where
    Update 0 a (l ': ls) = a ': ls
    Update n a (l ': ls) = l ': Update (n - 1) a ls


-- === Folds === --

type family Concat (lst :: [[k]]) :: [k] where
    Concat '[]           = '[]
    Concat (lst ': lsts) = Append lst (Concat lsts)


-- === Set like operations === --

type family Delete a lst where
    Delete a (a ': ls) = ls
    Delete a (l ': ls) = l ': Delete a ls
    Delete a '[] = '[]

type family UniqueInsert a lst where
    UniqueInsert a '[]       = '[a]
    UniqueInsert a (a ': as) = a ': as
    UniqueInsert a (b ': as) = b ': UniqueInsert a as

type family Unique lst where
    Unique '[]       = '[]
    Unique (a ': as) = UniqueInsert a (Unique as)


-- === Repeat lists === --

type family Replicate (n :: Nat) a where
    Replicate 0 a = '[]
    Replicate n a = a ': Replicate (n - 1) a


-- === Cartesian === --

type family CartesianWith (f :: k -> l -> m) (a :: [k]) (b :: [l]) :: [m] where
    CartesianWith f '[]       _  = '[]
    CartesianWith f (a ': as) ls = Append (CartesianWith__ f a  ls)
                                          (CartesianWith   f as ls)

type family CartesianWith__ (f :: k -> l -> m) (a :: k) (b :: [l]) :: [m] where
    CartesianWith__ _ _ '[]       = '[]
    CartesianWith__ f a (t ': ts) = f a t ': CartesianWith__ f a ts


-- === Zipping === --

type Zip  l1 l2          = Zip2             l1 l2
type Zip2 l1 l2          = ZipWith2 '(,)    l1 l2
type Zip3 l1 l2 l3       = ZipWith3 '(,,)   l1 l2 l3
type Zip4 l1 l2 l3 l4    = ZipWith4 '(,,,)  l1 l2 l3 l4
type Zip5 l1 l2 l3 l4 l5 = ZipWith5 '(,,,,) l1 l2 l3 l4 l5

type family ZipWith2 (f :: k1 -> k2 -> k) (l1 :: [k1]) (l2 :: [k2]) :: [k] where
    ZipWith2 f (t1 ': ts1) (t2 ': ts2) = f t1 t2 ': ZipWith2 f ts1 ts2
    ZipWith2 _ _ _ = '[]

type family ZipWith3 (f :: k1 -> k2 -> k3 -> k) (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) :: [k] where
    ZipWith3 f (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) = f t1 t2 t3 ': ZipWith3 f ts1 ts2 ts3
    ZipWith3 _ _ _ _ = '[]

type family ZipWith4 (f :: k1 -> k2 -> k3 -> k4 -> k) (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) (l4 :: [k4]) :: [k] where
    ZipWith4 f (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) (t4 ': ts4) = f t1 t2 t3 t4 ': ZipWith4 f ts1 ts2 ts3 ts4
    ZipWith4 _ _ _ _ _ = '[]

type family ZipWith5 (f :: k1 -> k2 -> k3 -> k4 -> k5 -> k) (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) (l4 :: [k4]) (l5 :: [k5]) :: [k] where
    ZipWith5 f (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) (t4 ': ts4) (t5 ': ts5) = f t1 t2 t3 t4 t5 ': ZipWith5 f ts1 ts2 ts3 ts4 ts5
    ZipWith5 _ _ _ _ _ _ = '[]



----------------------
-- === TreeList === --
----------------------

-- === Typed === --

data TreeList a
    = FlatList [a]
    | TreeList (TreeList a) (TreeList a)

type family FlattenTreeList (t :: TreeList a) :: [a] where
    FlattenTreeList ('FlatList a)   = a
    FlattenTreeList ('TreeList l r) = Append (FlattenTreeList l)
                                             (FlattenTreeList r)


-- === Dynamic === --

data DynTreeList (l :: Type) (r :: Type)

type family FlattenDynTreeList (lst :: Type) :: [Type] where
    FlattenDynTreeList (DynTreeList l r) =
        Append (FlattenDynTreeList l) (FlattenDynTreeList r)
    FlattenDynTreeList a = '[a]

