{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Data.RTuple where

import Prelude hiding (head, tail, map)
import Data.Typeable
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Type.Bool
import Control.Lens
import Control.Lens.Wrapped

----------------------------------------------------------------------------------
-- RTuple
----------------------------------------------------------------------------------

data RTuple a = RTuple { fromRTuple :: a } deriving (Eq, Ord)

data TList (t :: *) a = TList { fromTList :: a } deriving (Eq, Ord)


--type family 
--type family RTupData rt where RTupData (RTuple a) = a

--infixr 0 //
--(//) = (,)

-- === Wrapper ===

instance Wrapped (RTuple a) where
    type Unwrapped (RTuple a) = a
    _Wrapped' = iso fromRTuple RTuple

instance Wrapped (TList t a) where
    type Unwrapped (TList t a) = a
    _Wrapped' = iso fromTList TList


-- === Show ===

class ShowRtup r where
    showRtup :: String -> r -> String

instance ShowRtup () where
    showRtup _ _ = ""

instance Show a => ShowRtup (a,()) where
    showRtup _ (a,()) = show a

instance (Show a, ShowRtup as) => ShowRtup (a,as) where
    showRtup sep (a,as) = show a ++ sep ++ showRtup sep as

instance ShowRtup a => Show (RTuple a) where
    show (RTuple a) = "RTuple (" ++ showRtup ", " a ++ ")"

-- === Head ===

type family HeadOf a

type instance HeadOf (RTuple (r,rs)) = r
type instance HeadOf (TList a l)     = a

class Head a where
    head :: Lens' a (HeadOf a)

instance            Head (RTuple   (r,rs)) where head = lens (fst . view _Wrapped') (\(RTuple (_,rs)) r -> RTuple (r,rs))
instance (a ~ r) => Head (TList  a (r,rs)) where head = lens (fst . view _Wrapped') (\(TList  (_,rs)) r -> TList  (r,rs))

-- === Tail ===

type family TailOf a

type instance TailOf (RTuple  (r,rs)) = RTuple   rs
type instance TailOf (TList a (r,rs)) = TList  a rs

class Tail a where
    tail :: Lens' a (TailOf a)

instance Tail (RTuple (r,rs))  where tail = lens (\(RTuple (r,rs)) -> RTuple rs) (\(RTuple (r,_)) (RTuple rs) -> RTuple (r,rs))
instance Tail (TList a (r,rs)) where tail = lens (\(TList  (r,rs)) -> TList  rs) (\(TList  (r,_)) (TList  rs) -> TList  (r,rs))


-- === UncurryTuple ===

-- |converts function taking a tuple list as argument into standard haskell one
--  eg. `(a,(b,(c,()))) -> out` into `a -> b -> c -> out`
class UncurryTuple f out | f -> out where
    uncurryTuple :: f -> out

instance UncurryTuple (RTuple () -> a) a where
    uncurryTuple f = f $ RTuple ()

instance UncurryTuple (RTuple xs -> f) fout => UncurryTuple (RTuple (x,xs) -> f) (x -> fout) where
    uncurryTuple f = (\x -> uncurryTuple $ f . RTuple . (x,) . fromRTuple)


-- === ToRTup ===

type family AsRTuple (a :: k)


type instance AsRTuple ('[]       :: [*]) = RTuple ()
type instance AsRTuple ((l ': ls) :: [*]) = RTuple (l, Unwrapped (AsRTuple ls))


type family AsTListData t (a :: k)


type AsTList t lst = TList t (AsTListData t lst)

type instance AsTListData t ('[]       :: [*]) = ()
type instance AsTListData t ((l ': ls) :: [*]) = (t, Unwrapped (AsTList t ls))


-- === ToTuple ===

-- TODO [refactor to a tuple package]
type family AsTuple a
type instance AsTuple (RTuple ()) = ()
type instance AsTuple (RTuple (t1,())) = t1

class ToTuple a lst | a -> lst where
    toTuple :: a -> lst

instance ToTuple (RTuple ()) () where toTuple _ = ()
instance ToTuple (RTuple (t1,())) t1 where toTuple (RTuple (t1,())) = t1
instance ToTuple (RTuple (t1,(t2,()))) (t1,t2) where toTuple (RTuple (t1,(t2,()))) = (t1,t2)
instance ToTuple (RTuple (t1,(t2,(t3,())))) (t1,t2,t3) where toTuple (RTuple (t1,(t2,(t3,())))) = (t1,t2,t3)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,()))))) (t1,t2,t3,t4) where toTuple (RTuple (t1,(t2,(t3,(t4,()))))) = (t1,t2,t3,t4)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,())))))) (t1,t2,t3,t4,t5) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,())))))) = (t1,t2,t3,t4,t5)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))) (t1,t2,t3,t4,t5,t6) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))) = (t1,t2,t3,t4,t5,t6)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))) (t1,t2,t3,t4,t5,t6,t7) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))) = (t1,t2,t3,t4,t5,t6,t7)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))) (t1,t2,t3,t4,t5,t6,t7,t8) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,())))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,())))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,(t12,()))))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,(t12,()))))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)


-- === Elem indexing ===

type family ElTypeAt (idx :: Nat) a

class ElAt idx a where
    elAt :: Proxy idx -> Lens' a (ElTypeAt idx a)

class ElAt' idx t where
    elAt' :: Proxy idx -> Lens' (TList t lst) t

--

type instance ElTypeAt n (RTuple   (r,rs)) = If (n :== 0) r (ElTypeAt (n - 1) (RTuple  rs))
--type instance ElTypeAt n (TList  t (r,rs)) = If (n :== 0) t (ElTypeAt (n - 1) (TList t rs))
type instance ElTypeAt n (TList  t lst) = t

instance {-# OVERLAPPABLE #-} ( ElAt (n - 1) (RTuple rs)
                              , ElTypeAt n (RTuple (r,rs)) ~ ElTypeAt (n-1) (RTuple rs)
                              ) => ElAt n (RTuple (r,rs)) where elAt _ = tail . elAt (Proxy :: Proxy (n-1))
instance {-# OVERLAPPABLE #-} ElAt 0 (RTuple (r,rs)) where elAt _ = head


instance {-# OVERLAPPABLE #-}       ( ElAt (n - 1) (TList t rs)
                                    , ElTypeAt n (TList t (r,rs)) ~ ElTypeAt (n-1) (TList t rs)
                                    ) => ElAt n (TList t (r,rs)) where elAt _ = tail . elAt (Proxy :: Proxy (n-1))
instance {-# OVERLAPPABLE #-} (t ~ r) => ElAt 0 (TList t (r,rs)) where elAt _ = head


-- === RTup builder ===

--rtup = 

--class RTupBuilder b where
--    rtup :: 


--elAt0 :: ElTypeAt 0 a => a -> ElAt 0 a
--elAt0 = ElTypeAt (Proxy :: Proxy 0)

--elAt1 :: ElTypeAt 1 a => a -> ElAt 1 a
--elAt1 = ElTypeAt (Proxy :: Proxy 1)

--elAt2 :: ElTypeAt 2 a => a -> ElAt 2 a
--elAt2 = ElTypeAt (Proxy :: Proxy 2)

--elAt3 :: ElTypeAt 3 a => a -> ElAt 3 a
--elAt3 = ElTypeAt (Proxy :: Proxy 3)

--elAt4 :: ElTypeAt 4 a => a -> ElAt 4 a
--elAt4 = ElTypeAt (Proxy :: Proxy 4)

--elAt5 :: ElTypeAt 5 a => a -> ElAt 5 a
--elAt5 = ElTypeAt (Proxy :: Proxy 5)

--elAt6 :: ElTypeAt 6 a => a -> ElAt 6 a
--elAt6 = ElTypeAt (Proxy :: Proxy 6)

--elAt7 :: ElTypeAt 7 a => a -> ElAt 7 a
--elAt7 = ElTypeAt (Proxy :: Proxy 7)

--elAt8 :: ElTypeAt 8 a => a -> ElAt 8 a
--elAt8 = ElTypeAt (Proxy :: Proxy 8)

--elAt9 :: ElTypeAt 9 a => a -> ElAt 9 a
--elAt9 = ElTypeAt (Proxy :: Proxy 9)
