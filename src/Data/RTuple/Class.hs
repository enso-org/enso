{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeInType              #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Data.RTuple.Class where

import           Prelude hiding (map, init, last, length, take, concat, mapM, head, tail, reverse, drop, zip, zip3, unzip, unzip3)
import qualified Prelude as P

import Control.Lens.Utils     hiding (Index, cons, uncons, reversed, index)
import Control.Monad.Identity hiding (mapM)
import Data.Default
import Data.List              (intercalate)
import Data.Typeable
import GHC.Exts               (Any, unsafeCoerce#)
import GHC.TypeLits
import Type.Container         (Append, Index, Index2, Reverse)
import Type.Monoid
import Type.List              hiding (Empty)
-- import Data.Construction
import Type.Bool              (If, type (==))
import Data.Proxify
import Data.Property hiding (Update, update)
import qualified Data.Property as Prop
import Data.Kind

-- === Definition === --

    --newtype RTuple (t :: [*]) = RTuple (Lst2RT t)

    ---- === Helpers === --

    --type family Lst2RT lst where
    --    Lst2RT '[]       = ()
    --    Lst2RT (a ': as) = (a, Lst2RT as)


    ---- === Instances === --

    ---- Show
    --deriving instance Show (Lst2RT t) => Show (RTuple t)

    ---- Wrappers
    --makeWrapped ''RTuple

    ---- Type concat
    --type instance Concat (RTuple a) (RTuple b) = RTuple (a <> b)


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


class Empty a where
    empty :: a

-- === Definition === --

infixr 5 :-:
data List lst where
    Null  :: List '[]
    (:-:) :: a -> List lst -> List (a ': lst)


-- === Type instance === --

type instance Concat (List a) (List b) = List (a <> b)


-- === Instnaces === --

instance lst ~ '[] => Empty (List lst) where
    empty = Null ; {-# INLINE empty #-}


-- === Utils === --

-- Info

null :: List lst -> Bool
null Null = True
null _    = False
{-# INLINE null #-}

single :: List lst -> Bool
single (_ :-: Null) = True
single _             = False
{-# INLINE single #-}

length :: List lst -> Int
length Null         = 0
length (_ :-: lst) = succ $ length lst
{-# INLINE length #-}


-- Construction

singleton :: a -> List '[a]
singleton a = a :-: Null
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
    fromList' (a : as) = unsafeCoerce# $ a :-: (unsafeCoerce# $ fromList' as)
    {-# INLINE fromList' #-}

{-# INLINE fromList #-}


class Monad m => Generate ctx m t where
    generate :: Proxy ctx -> (forall a. ctx m a => m a) -> m t

instance Monad m => Generate ctx m (List '[]) where
    generate _ _ = return empty ; {-# INLINE generate #-}

instance (Monad m, ctx m l, Generate ctx m (List ls)) => Generate ctx m (List (l ': ls)) where
    generate ctx g = (:-:) <$> g <*> generate ctx g ; {-# INLINE generate #-}

-- Modification

concat :: List xs -> List ys -> List (xs <> ys)
concat Null       ys = ys
concat (x :-: xs) ys = x :-: concat xs ys
{-# INLINE concat #-}

append :: a -> List lst -> List (Append a lst)
append a Null       = a :-: Null
append a (l :-: ls) = l :-: append a ls
{-# INLINE append #-}

prepend :: a -> List lst -> List (a ': lst)
prepend = cons
{-# INLINE prepend #-}

-- insert


-- Transformations --

cons :: a -> List lst -> List (a ': lst)
cons = (:-:)
{-# INLINE cons #-}

uncons :: List (x ': xs) -> (x, List xs)
uncons = view unconsed
{-# INLINE uncons #-}

unconsed  :: Iso  (List (x ': xs)) (List (y ': ys)) (x, List xs) (y, List ys)
unconsed' :: Iso' (List (x ': xs))                  (x, List xs)
unconsed  = iso (\(x :-: xs) -> (x,xs)) (\(x,xs) -> x :-: xs)
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
takeInit Null               = Null
takeInit (x :-: Null)       = Null
takeInit (x :-: (y :-: ys)) = x :-: (takeInit $ y :-: ys)
{-# INLINE takeInit #-}

dropInit :: List lst -> List (DropInit lst)
dropInit Null               = Null
dropInit (x :-: Null)       = singleton x
dropInit (x :-: (y :-: ys)) = dropInit $ y :-: ys
{-# INLINE dropInit #-}

last  :: Lens  (List lst) (List (Append x (Init lst))) (Last lst) x
last' :: Lens' (List lst)                              (Last lst)
last  = lens takeLast (\lst nlast ->                 append nlast (takeInit lst))
last' = lens takeLast (\lst nlast -> unsafeCoerce# $ append nlast (takeInit lst))
{-# INLINE last  #-}
{-# INLINE last' #-}

takeLast :: List lst -> Last lst
takeLast (x :-: Null)       = x
takeLast (x :-: (y :-: ys)) = takeLast $ y :-: ys
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
    reverse' Null       lst = lst
    reverse' (l :-: ls) lst = reverse' ls (l :-: lst)
    {-# INLINE reverse' #-}
{-# INLINE reverse #-}



-- Sublists --

take :: KnownNat n => Proxy (n :: Nat) -> List lst -> List (Take n lst)
take = unsafeCoerce# . take' . fromIntegral . natVal where
    take' :: Int -> List lst -> Any
    take' 0 _          = unsafeCoerce# Null
    take' n (l :-: ls) = unsafeCoerce# $ l :-: (unsafeCoerce# $ take' (pred n) ls)
    {-# INLINE take' #-}
{-# INLINE take #-}

drop :: KnownNat n => Proxy (n :: Nat) -> List lst -> List (Drop n lst)
drop = unsafeCoerce# . drop' . fromIntegral . natVal where
    drop' :: Int -> List lst -> Any
    drop' 0 lst        = unsafeCoerce# lst
    drop' n (_ :-: ls) = drop' (pred n) ls
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
    index' 0 (a :-: _) = unsafeCoerce# a
    index' i (_ :-: l) = index' (pred i) l
    {-# INLINE index' #-}
{-# INLINE index #-}

update :: KnownNat n => Proxy (n :: Nat) -> a -> List lst -> List (Update n a lst)
update idx = unsafeCoerce# . update' (fromIntegral $ natVal idx) where
    update' :: Int -> a -> List lst -> Any
    update' 0 a (_ :-: l) = unsafeCoerce# $ a :-: l
    update' n a (x :-: l) = unsafeCoerce# $ x :-: (unsafeCoerce# $ update' (pred n) a l)
    {-# INLINE update' #-}
{-# INLINE update #-}

indexed :: KnownNat n => Proxy (n :: Nat) -> Lens (List lst) (List (Update n a lst)) (Index2 n lst) a
indexed i = lens (index i) (flip $ update i)
{-# INLINE indexed #-}


-- === Zipping === --

zip :: List lst -> List lst' -> List (Zip lst lst')
zip = zip2
{-# INLINE zip #-}

zip2 :: List lst1 -> List lst2 -> List (Zip2 lst1 lst2)
zip2 (x1 :-: xs1) (x2 :-: xs2) = (x1,x2) :-: zip2 xs1 xs2
zip2 Null          _           = Null
zip2 _             Null        = Null
{-# INLINE zip2 #-}

zip3 :: List lst1 -> List lst2 -> List lst3 -> List (Zip3 lst1 lst2 lst3)
zip3 (x1 :-: xs1) (x2 :-: xs2) (x3 :-: xs3) = (x1,x2,x3) :-: zip3 xs1 xs2 xs3
zip3 Null          _             _          = Null
zip3 _             Null          _          = Null
zip3 _             _             Null       = Null
{-# INLINE zip3 #-}

zip4 :: List lst1 -> List lst2 -> List lst3 -> List lst4 -> List (Zip4 lst1 lst2 lst3 lst4)
zip4 (x1 :-: xs1) (x2 :-: xs2) (x3 :-: xs3) (x4 :-: xs4) = (x1,x2,x3,x4) :-: zip4 xs1 xs2 xs3 xs4
zip4 Null          _             _             _         = Null
zip4 _             Null          _             _         = Null
zip4 _             _             Null          _         = Null
zip4 _             _             _             Null      = Null
{-# INLINE zip4 #-}

zip5 :: List lst1 -> List lst2 -> List lst3 -> List lst4 -> List lst5 -> List (Zip5 lst1 lst2 lst3 lst4 lst5)
zip5 (x1 :-: xs1) (x2 :-: xs2) (x3 :-: xs3) (x4 :-: xs4) (x5 :-: xs5) = (x1,x2,x3,x4,x5) :-: zip5 xs1 xs2 xs3 xs4 xs5
zip5 Null          _             _             _             _        = Null
zip5 _             Null          _             _             _        = Null
zip5 _             _             Null          _             _        = Null
zip5 _             _             _             Null          _        = Null
zip5 _             _             _             _             Null     = Null
{-# INLINE zip5 #-}


-- TODO[WD]: Unzipping is not straight-forward because we have to ensure Haskell's TC that elements are tuples!

--type family   ListTuple (t :: k)
--type instance ListTuple '(l1, l2)             = (List l1, List l2)
--type instance ListTuple '(l1, l2, l3)         = (List l1, List l2, List l3)
--type instance ListTuple '(l1, l2, l3, l4)     = (List l1, List l2, List l3, List l4)
--type instance ListTuple '(l1, l2, l3, l4, l5) = (List l1, List l2, List l3, List l4, List l5)

--unzip2 :: List lst -> ListTuple (Unzip2 lst)
--unzip2 Null = (Null, Null)
--unzip2 (Cons (x1,x2) xs) = let (xs1, xs2) = unzip2 xs
--                           in (Cons x1 xs1, Cons x2 xs2)


class                        Focus lst       a where focus :: Lens' (List lst) a
instance {-# OVERLAPPING #-} Focus (a ': ls) a where focus = head         ; {-# INLINE focus #-}
instance Focus ls a       => Focus (l ': ls) a where focus = tail . focus ; {-# INLINE focus #-}


-- === Instances === --

-- Show
type ShowElems = Map Show
instance ShowElems lst => Show (List lst) where
    show lst = '[' : intercalate "," (map (Proxy :: Proxy Show) show lst) ++ "]"

-- Default
instance Default (List '[])                                         where def = Null        ; {-# INLINE def #-}
instance (Default l, Default (List ls)) => Default (List (l ': ls)) where def = def :-: def ; {-# INLINE def #-}





-- === Mapping === --

class    ctx a => CtxM ctx a (m :: ★ -> ★)
instance ctx a => CtxM ctx a m

type  Map  ctx = MapM (CtxM ctx) Identity
class MapM ctx m lst where
    mapM :: Proxy ctx -> (forall a. ctx a m => a -> m b) -> List lst -> m [b]

map :: forall ctx lst a b. Map ctx lst => Proxy ctx -> (forall a. ctx a => a -> b) -> List lst -> [b]
map _ f lst = runIdentity $ mapM (Proxy :: Proxy (CtxM ctx)) (return . f) lst
{-# INLINE map #-}

instance Monad m                           => MapM ctx m '[]       where mapM _ _ _            = return []                     ; {-# INLINE mapM #-}
instance (Monad m, ctx l m, MapM ctx m ls) => MapM ctx m (l ': ls) where mapM ctx f (l :-: ls) = (:) <$> f l <*> mapM ctx f ls ; {-# INLINE mapM #-}







--------------------------------------------------------------

-- data Assoc a b = a :~ b
--
-- type family AssocKey a where AssocKey (a :~ b) = a
-- type family AssocVal a where AssocVal (a :~ b) = b
-- type family AssocKeys as where
--             AssocKeys '[] = '[]
--             AssocKeys (a ': as) = AssocKey a ': AssocKeys as
--
-- type family AssocVals as where
--             AssocVals '[] = '[]
--             AssocVals (a ': as) = AssocVal a ': AssocVals as
--
-- -- newtype TMap2 (assocs :: [Assoc]) = TMap2 (TMap (AssocKeys assocs) (AssocVals assocs))



data Cycle a = Cycle a

data Assoc key val = key := val deriving (Show, Functor, Traversable, Foldable)

type keys :=: vals = Assocs keys vals



type family AssocElem lst where
    AssocElem [a]       = a
    AssocElem (Cycle a) = a

type family   Assocs (keys :: k) (vals :: v) :: [Assoc (AssocElem k) (AssocElem v)]
type instance Assocs '[]         _            = '[]
type instance Assocs (k ': ks)   (v ': vs)    = k ':= v ': Assocs ks vs
type instance Assocs (k ': ks)   ('Cycle v)   = k ':= v ': Assocs ks ('Cycle v)
type instance Assocs ('Cycle k)  (v ': vs)    = k ':= v ': Assocs ('Cycle k) vs

-- type keys ':=> vals = Assocs keys vals

type family SetAssoc (k :: kk) (v :: vk) (rels :: [Assoc kk vk]) :: [Assoc kk vk] where
    SetAssoc k v ((k ':= v') ': as) = (k ':= v) ': as
    SetAssoc k v ((l ':= v)  ': as) = (l ':= v) ': SetAssoc k v as

type family IndexOf' a lst where
    IndexOf' a (a ': ls) = 0
    IndexOf' a (l ': ls) = 1 + IndexOf' a ls


type family MapVal (f :: v -> v') (rels :: Assoc k v) :: Assoc k v' where
            MapVal f (k ':= v) = k ':= f v

type family MapVals (f :: v -> v') (rels :: [Assoc k v]) :: [Assoc k v'] where
            MapVals f '[]       = '[]
            MapVals f (a ': as) = MapVal f a ': MapVals f as

type family ReplaceVals (nv :: v') (rels :: [Assoc k v]) :: [Assoc k v'] where
            ReplaceVals v' '[]             = '[]
            ReplaceVals v' ((k ':= v) ': as) = (k ':= v') ': ReplaceVals v' as


type family Key (rel :: Assoc k v) :: v where
            Key (key ':= val) = key

type family Keys (rels :: [Assoc k v]) :: [k] where
            Keys '[] = '[]
            Keys (r ': rs) = Key r ': Keys rs


type family Value (rel :: Assoc k v) :: v where
            Value (key ':= val) = val

type family Values (rels :: [Assoc k v]) :: [v] where
            Values '[] = '[]
            Values (r ': rs) = Value r ': Values rs

type family LookupAssoc k s where
            LookupAssoc k '[]            = 'Nothing
            LookupAssoc k (k ':= v ': _) = 'Just v
            LookupAssoc k (l ':= _ ': s) = LookupAssoc k s

--newtype TMap (keys :: [k]) (vals :: [*]) = TMap (List vals)

-- newtype TMap (keys :: [k]) (vals :: [*]) = TMap (List vals)
-- makeWrapped ''TMap

newtype TMap (rels :: [Assoc k ★]) = TMap (List (Values rels))
makeWrapped ''TMap

type family Relations a where
    Relations (TMap rels) = rels

-- === Instances === --

instance (rels ~ '[]) => Empty (TMap rels) where
    empty = TMap empty ; {-# INLINE empty #-}

-- === Utils === --


insert2 :: Proxy (key :: k) -> val -> TMap rels -> TMap (key ':= val ': rels)
insert2 _ val = wrapped %~ prepend val
{-# INLINE insert2 #-}

-- Access

-- type family Access (key :: k) (rels :: [Assoc k v]) :: v where
--     Access k (k ':= v ': rels) = v
--     Access k (l ':= v ': rels) = Access k rels
--
-- type family Access2 (key :: k) (keys :: [k]) (vals :: [v]) :: v where
--     Access2 k (k ': ks) (v ': vs) = v
--     Access2 k (l ': ks) (v ': vs) = Access2 k ks vs


-- type family Access (key :: *) a
-- class Accessible' (key :: *) a where accessProxy' :: Proxy key -> Lens' a (Access key a)
--
-- access' :: (Accessible' key a, key ~ Proxified k) => k -> Lens' a (Access key a)
-- access' = accessProxy' . proxify ; {-# INLINE access' #-}
--
-- type instance Access key (TMap (k ': ks) (v ': vs)) = If (key == k) v (Access key (TMap ks vs))

type instance Access (key :: ★)   (TMap (k ':= v ': rels)) = If (key == k) v (Access key (TMap rels))
type instance Prop.Update key a (TMap (k ':= v ': rels)) = If (key == k) (TMap (k ':= a ': rels)) (TMap (k ':= v ': (Relations (Prop.Update key a (TMap rels)))))

instance {-# OVERLAPPABLE #-} (GetMissmatch k (l ':= v ': rels), Accessor k (TMap rels))
                           => Accessor k (TMap (l ':= v ': rels)) where access = access @k . view tail2    ; {-# INLINE access #-}
instance {-# OVERLAPPABLE #-} Accessor k (TMap (k ':= v ': rels)) where access = view (wrapped' . head) ; {-# INLINE access #-}

instance {-# OVERLAPPABLE #-} (GetMissmatch k (l ':= v ': rels), Updater' k (TMap rels), Prop.Update k (Access k (TMap (l ':= v : rels))) (TMap (l ':= v : rels)) ~ TMap (l ':= v : rels), Prop.Update k (Access k (TMap rels)) (TMap rels) ~ TMap rels)
                           => Updater' k (TMap (l ':= v ': rels)) where update' a = tail2 %~ update' @k a     ; {-# INLINE update' #-}
instance {-# OVERLAPPABLE #-} Updater' k (TMap (k ':= v ': rels)) where update' a = (wrapped' . head) .~ a ; {-# INLINE update' #-}


-- type family AccessMissmatch k ks vs where
--     AccessMissmatch k (l ': ks) (v ': vs) = Access k (TMap (l ': ks) (v ': vs))
--                                           ~ Access k (TMap       ks        vs)

type family GetMissmatch k rels where
    GetMissmatch k (l ':= v ': rels) = Access k (TMap (l ':= v ': rels))
                                     ~ Access k (TMap rels)

-- instance {-# OVERLAPPABLE #-} (AccessMissmatch k (l ': ks) (v ': vs), Accessible' k (TMap ks vs))
--                            => Accessible' k (TMap (l ': ks) (v ': vs)) where accessProxy' k = tail2 . accessProxy' k ; {-# INLINE accessProxy' #-}
-- instance {-# OVERLAPPABLE #-} Accessible' k (TMap (k ': ks) (v ': vs)) where accessProxy' _ = wrapped' . head'       ; {-# INLINE accessProxy' #-}


-- class Accessible (key :: k) (keys :: [k]) (vals :: [*]) where access :: Proxy key -> Lens' (TMap keys vals) (Access2 key keys vals)
--
-- instance {-# OVERLAPPABLE #-} (Access2 k (l ': ks) (v ': vs) ~ Access2 k ks vs, Accessible k ks vs)
--                            => Accessible k (l ': ks) (v ': vs) where access k = tail2 . access k ; {-# INLINE access #-}
-- instance {-# OVERLAPPABLE #-} Accessible k (k ': ks) (v ': vs) where access _ = wrapped' . head  ; {-# INLINE access #-}

-- instance {-# OVERLAPPABLE #-} (Access k (l ':= v ': rels) ~ Access k rels, Accessible k rels)
--                            => Accessible k (l ':= v ': rels) where access k = tail2 . access k ; {-# INLINE access #-}
-- instance {-# OVERLAPPABLE #-} Accessible k (k ':= v ': rels) where access _ = wrapped' . head  ; {-# INLINE access #-}


tail2 :: Lens' (TMap (r ': rels)) (TMap rels)
tail2 = wrapped . tail . from wrapped ; {-# INLINE tail2 #-}

prepend2 :: v -> TMap rels -> TMap (k ':= v ': rels)
prepend2 a = wrapped %~ prepend a ; {-# INLINE prepend2 #-}

-- -- === Instances === --

deriving instance Show (Unwrapped (TMap rels)) => Show (TMap rels)



--
--
--
-- -- === Instances === --
--
-- instance (keys ~ '[], vals ~ '[]) => Empty (TMap keys vals) where
--     empty = TMap empty ; {-# INLINE empty #-}
--
-- -- === Utils === --
--
-- empty2 :: TMap '[] '[]
-- empty2 = TMap empty
-- {-# INLINE empty2 #-}
--
-- insert2 :: Proxy (key :: k) -> val -> TMap keys vals -> TMap (key ': keys) (val ': vals)
-- insert2 _ val = wrapped %~ prepend val
-- {-# INLINE insert2 #-}
--
-- -- Access
--
-- -- type family Access (key :: k) (rels :: [Assoc k v]) :: v where
-- --     Access k (k ':= v ': rels) = v
-- --     Access k (l ':= v ': rels) = Access k rels
-- --
-- -- type family Access2 (key :: k) (keys :: [k]) (vals :: [v]) :: v where
-- --     Access2 k (k ': ks) (v ': vs) = v
-- --     Access2 k (l ': ks) (v ': vs) = Access2 k ks vs
--
--
-- type family Access (key :: *) a
-- class Accessible' (key :: *) a where accessProxy' :: Proxy key -> Lens' a (Access key a)
--
-- access' :: (Accessible' key a, key ~ Proxified k) => k -> Lens' a (Access key a)
-- access' = accessProxy' . proxify ; {-# INLINE access' #-}
--
-- type instance Access key (TMap (k ': ks) (v ': vs)) = If (key == k) v (Access key (TMap ks vs))
--
-- type instance Access key (TMap (k ': ks) (v ': vs)) = If (key == k) v (Access key (TMap ks vs))
--
-- instance {-# OVERLAPPABLE #-} (GetMissmatch k (l ': ks) (v ': vs), Accessor k (TMap ks vs))
--                            => Accessor k (TMap (l ': ks) (v ': vs)) where access = access @k . view tail2    ; {-# INLINE access #-}
-- instance {-# OVERLAPPABLE #-} Accessor k (TMap (k ': ks) (v ': vs)) where access = view (wrapped' . head) ; {-# INLINE access #-}
--
--
-- type family AccessMissmatch k ks vs where
--     AccessMissmatch k (l ': ks) (v ': vs) = Access k (TMap (l ': ks) (v ': vs))
--                                           ~ Access k (TMap       ks        vs)
--
-- type family GetMissmatch k ks vs where
--     GetMissmatch k (l ': ks) (v ': vs) = Access k (TMap (l ': ks) (v ': vs))
--                                        ~ Access k (TMap       ks        vs)
--
-- instance {-# OVERLAPPABLE #-} (AccessMissmatch k (l ': ks) (v ': vs), Accessible' k (TMap ks vs))
--                            => Accessible' k (TMap (l ': ks) (v ': vs)) where accessProxy' k = tail2 . accessProxy' k ; {-# INLINE accessProxy' #-}
-- instance {-# OVERLAPPABLE #-} Accessible' k (TMap (k ': ks) (v ': vs)) where accessProxy' _ = wrapped' . head'       ; {-# INLINE accessProxy' #-}
--
--
-- -- class Accessible (key :: k) (keys :: [k]) (vals :: [*]) where access :: Proxy key -> Lens' (TMap keys vals) (Access2 key keys vals)
-- --
-- -- instance {-# OVERLAPPABLE #-} (Access2 k (l ': ks) (v ': vs) ~ Access2 k ks vs, Accessible k ks vs)
-- --                            => Accessible k (l ': ks) (v ': vs) where access k = tail2 . access k ; {-# INLINE access #-}
-- -- instance {-# OVERLAPPABLE #-} Accessible k (k ': ks) (v ': vs) where access _ = wrapped' . head  ; {-# INLINE access #-}
--
-- -- instance {-# OVERLAPPABLE #-} (Access k (l ':= v ': rels) ~ Access k rels, Accessible k rels)
-- --                            => Accessible k (l ':= v ': rels) where access k = tail2 . access k ; {-# INLINE access #-}
-- -- instance {-# OVERLAPPABLE #-} Accessible k (k ':= v ': rels) where access _ = wrapped' . head  ; {-# INLINE access #-}
--
--
-- tail2 :: Lens' (TMap (k ': ks) (v ': vs)) (TMap ks vs)
-- tail2 = wrapped . tail . from wrapped
-- {-# INLINE tail2 #-}
-- --
-- --
-- prepend2 :: v -> TMap ks vs -> TMap (k ': ks) (v ': vs)
-- prepend2 a = wrapped %~ prepend a
-- --
-- --
-- -- -- === Instances === --
-- --
-- deriving instance Show (Unwrapped (TMap ks vs)) => Show (TMap ks vs)
-- --
-- -- --prepend :: a -> List lst -> List (a ': lst)
-- -- --prepend = cons
-- -- --{-# INLINE prepend #-}
-- --
-- --
-- -- instance (Creator m (Unwrapped (TMap rels)), Monad m) => Creator m (TMap rels) where
-- --     create = TMap <$> create ; {-# INLINE create #-}
-- --
-- -- instance Monad m                            => Creator m (List '[])       where create = return Null                ; {-# INLINE create #-}
-- -- instance (Creator m l, Creator m (List ls)) => Creator m (List (l ': ls)) where create = Cons <$> create <*> create ; {-# INLINE create #-}
--
--
--
-- -- data List lst where
-- --     Null :: List '[]
-- --     Cons :: a -> List lst -> List (a ': lst)
