{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.List (module Type.Data.List) where

import Prelude
import GHC.TypeLits
import Type.Data.Bool
import Type.Data.Maybe
import GHC.TypeLits



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

type family Append (lst :: [k]) (lst' :: [k]) :: [k] where
    Append '[]       lst = lst
    Append (l ': ls) lst = l ': Append ls lst



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



-- === Repeat lists === --

type family Replicate (n :: Nat) a where
    Replicate 0 a = '[]
    Replicate n a = a ': Replicate (n - 1) a



-- === Zipping === --

type Zip a b = Zip2 a b
type family Zip2 (l1 :: [k1]) (l2 :: [k2]) :: [(k1,k2)] where
    Zip2 (t1 ': ts1) (t2 ': ts2) = '(t1,t2) ': Zip2 ts1 ts2
    Zip2 _ _ = '[]

type family Zip3 (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) :: [(k1,k2,k3)] where
    Zip3 (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) = '(t1,t2,t3) ': Zip3 ts1 ts2 ts3
    Zip3 _ _ _ = '[]

type family Zip4 (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) (l4 :: [k4]) :: [(k1,k2,k3,k4)] where
    Zip4 (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) (t4 ': ts4) = '(t1,t2,t3,t4) ': Zip4 ts1 ts2 ts3 ts4
    Zip4 _ _ _ _ = '[]

type family Zip5 (l1 :: [k1]) (l2 :: [k2]) (l3 :: [k3]) (l4 :: [k4]) (l5 :: [k5]) :: [(k1,k2,k3,k4,k5)] where
    Zip5 (t1 ': ts1) (t2 ': ts2) (t3 ': ts3) (t4 ': ts4) (t5 ': ts5) = '(t1,t2,t3,t4,t5) ': Zip5 ts1 ts2 ts3 ts4 ts5
    Zip5 _ _ _ _ _ = '[]
