{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Container.Poly where

--import Prologue hiding (Ixed, Indexed, Simple)
--import           Data.TypeLevel.List (In)
--import Data.Typeable
--import GHC.Prim
--import Type.Bool
import qualified Data.Container.Opts as Mods
--import           Data.Container.Mods (FilterMutable, filterMutable)



--type family AppendLst a lst where AppendLst a '[]       = '[a]
--                                  AppendLst a (l ': ls) = l ': AppendLst a ls



--------------------

--type family CmpLst (lst :: [k]) (lst' :: [k']) :: [Bool] where
--            CmpLst (l ': ls)    (l  ': ls')     = (True  ': CmpLst ls ls')
--            CmpLst (l ': ls)    (l' ': ls')     = (False ': CmpLst ls ls')
--            CmpLst '[]          '[]             = '[]

--type family LstIn (lst :: [*]) (lst' :: [*]) :: [Bool] where
--            LstIn (l ': ls) lst = In l lst ': LstIn ls (Remove l lst)
--            LstIn ls        '[] = '[]
--            LstIn '[]       lst = '[]


--type family   Remove (a :: k) (cont :: c) :: c
--type instance Remove (a :: k) ((t ': ts) :: [k]) = If (a :== t) ts (t ': Remove a ts)
--type instance Remove (a :: k) '[]                = '[]


--type family ModsOf (inst :: k) cont :: [*]

--data InstMods (inst :: k) (mods :: [Bool]) = InstMods
--data InstMods2 (inst :: k) (mods :: [Bool]) (cont :: *) = InstMods2
--data InstModsX (inst :: k) (query :: [*]) (mods :: [Bool]) (cont :: *) = InstModsX








--newtype Tagged t a = Tagged { fromTag :: a } deriving (Show)

--tagged :: Proxy t -> a -> Tagged t a
--tagged _ = Tagged

--type family FillData (layout :: [*]) datas where FillData (t ': ts) datas = (GetData t datas, FillData ts datas)
--                                                 FillData '[]       datas = ()

--type family GetData (tag :: *) datas where GetData tag (Tagged tag  a, ds) = a
--                                           GetData tag (Tagged tag' a, ds) = GetData tag ds

--type family   TaggedCont (tags :: [*]) a
--type instance TaggedCont '[]       ()     = ()
--type instance TaggedCont (t ': ts) (a,as) = (Tagged t a, TaggedCont ts as)

--class Taggable tags cont where
--    taggedCont :: Proxy tags -> cont -> TaggedCont tags cont

--instance {-# OVERLAPPABLE #-}                   Taggable '[]       ()     where taggedCont _ _ = ()
--instance {-# OVERLAPPABLE #-} Taggable ts as => Taggable (t ': ts) (a,as) where taggedCont _ (a,as) = (Tagged a, taggedCont (Proxy :: Proxy ts) as)

--class                                                                          DataFillable (layout :: [*]) datas where fillData :: Proxy layout -> datas -> FillData layout datas
--instance {-# OVERLAPPABLE #-} (DataGettable t datas, DataFillable ts datas) => DataFillable (t ': ts) datas where fillData _ ds = (getData (Proxy :: Proxy t) ds, fillData (Proxy :: Proxy ts) ds)
--instance {-# OVERLAPPABLE #-}                                                  DataFillable '[]       datas where fillData _ _  = ()

--class                                                                                                    DataGettable tag datas               where getData :: Proxy tag -> datas -> GetData tag datas
--instance {-# OVERLAPPABLE #-}                                                                            DataGettable tag (Tagged tag  a, ds) where getData _ = fromTag . fst
--instance {-# OVERLAPPABLE #-} (DataGettable tag ds, GetData tag (Tagged tag' a, ds) ~ GetData tag ds) => DataGettable tag (Tagged tag' a, ds) where getData t = getData t . snd



--type family   MappedByTag tag a l
--type instance MappedByTag t a ()                 = ()
--type instance MappedByTag t a (Tagged t' a', ls) = If (t :== t') (Tagged t  a , MappedByTag t a ls)
--                                                                 (Tagged t' a', MappedByTag t a ls)

--class                                                                                                                       MapByTag  t a b l       | t l -> a where mapByTag  :: Proxy t -> (a -> b) -> l -> MappedByTag t b l
--class                                                                                                                       MapByTag' t a b l                  where mapByTag' :: Proxy t -> (a -> b) -> l -> MappedByTag t b l
--instance {-# OVERLAPPABLE #-} (MapByTag' t a b l, a ~ a')                                                                => MapByTag  t a b (Tagged t  a', l ) where mapByTag  t f (Tagged a, l) = (Tagged $ f a, mapByTag' t f l)
--instance {-# OVERLAPPABLE #-} (MapByTag  t a b l, MappedByTag t b (Tagged t' a', l) ~ (Tagged t' a', MappedByTag t b l)) => MapByTag  t a b (Tagged t' a', l ) where mapByTag  t f (ta      , l) = (ta          , mapByTag  t f l)
--instance {-# OVERLAPPABLE #-} (MapByTag' t a b l, a ~ a')                                                                => MapByTag' t a b (Tagged t  a', l ) where mapByTag' t f (Tagged a, l) = (Tagged $ f a, mapByTag' t f l)
--instance {-# OVERLAPPABLE #-} (MapByTag' t a b l, MappedByTag t b (Tagged t' a', l) ~ (Tagged t' a', MappedByTag t b l)) => MapByTag' t a b (Tagged t' a', l ) where mapByTag' t f (ta      , l) = (ta          , mapByTag' t f l)
--instance                                                                                                                    MapByTag' t a b ()                 where mapByTag' _ _ _ = ()




--type I = InstMods2
--type I2X = InstModsX

--type family UpdateQuery instMods guery where
--    UpdateQuery (InstModsX inst q m cont) q'= InstModsX inst q' (LstIn (ModsOf inst cont) q') cont





--rebaseSpecX :: InstModsX inst q mods cont -> InstModsX inst' q (LstIn (ModsOf inst' cont) q) cont
--rebaseSpecX _ = InstModsX

--polySpecX :: InstModsX inst q mods cont -> InstModsX inst q (LstIn (ModsOf inst cont') q) cont'
--polySpecX _ = InstModsX
----type family Rebase mods base where Rebase (InstMods2 inst mods old) new = InstMods2 inst mods new

--type family InstQuery (inst :: * -> Constraint) :: [*]

--query :: InstModsX inst q mods cont -> Proxy q
--query _ = Proxy







--optBuilder :: a -> OptBuilderBase a
--optBuilder = OptBuilder

--type OptBuilderBase = OptBuilder '[]



--newtype OptBuilder (opts :: [*]) a = OptBuilder a deriving (Functor)

--class FuncBuilder f a | a -> f where
--    buildFunc :: f -> a

--class FuncTrans opts f a | a opts -> f where
--    transFunc :: OptBuilder opts f -> a

--instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)             => FuncBuilder (f -> g)            (a -> b)            where buildFunc = id
--instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (f -> g)            (OptBuilder opts t) where buildFunc = OptBuilder

--instance                            (opts ~ opts', f ~ f')       => FuncTrans opts f  (OptBuilder opts' f') where transFunc = id
--instance                          (f ~ (Proxy opts -> a -> b))   => FuncTrans opts f  (a -> b)              where transFunc (OptBuilder f) = f Proxy





--extendOptBuilder :: Proxy opt -> Proxy opts' -> OptBuilder opts a -> OptBuilder (opt ': (Concat opts' opts)) a
--extendOptBuilder _ _ (OptBuilder a) = OptBuilder a




----------------





--type family Selected (b :: [Bool]) (lst :: [*]) :: [*] where
--    Selected ('True  ': b) (l ': ls) = l ': Selected b ls
--    Selected ('False ': b) (l ': ls) =      Selected b ls
--    Selected '[]           lst       =      '[]
--    Selected s             lst       =      '[]



--data Query (q :: [*]) (m :: [Bool]) = Query


--data NA = NA
--data Info (idx :: *) (el :: *) (cls :: k) (cont :: *) = Info

--type family InfoIdx  i where InfoIdx  (Info idx el cls cont) = idx
--type family InfoEl   i where InfoEl   (Info idx el cls cont) = el
--type family InfoCls  i where InfoCls  (Info idx el cls cont) = cls
--type family InfoCont i where InfoCont (Info idx el cls cont) = cont

--type RawInfo           = Info NA  NA
--type IxedInfo   idx    = Info idx NA
--type ElInfo         el = Info NA  el
--type IxedElInfo idx el = Info idx el



--type family ElementOf        cont
--type family IndexOf      el  cont
--type family HomoIndexOf (m :: * -> *) :: *
--type family ElementByIx idx cont
--type family IxType      idx

--type IndexOf' cont = IndexOf (ElementOf cont) cont

---- === Results ===



--type family SelTags (info :: *) (s :: [Bool]) where SelTags (Info idx el cls cont) s = QueryData (Info idx el cls (DataStoreOf cont)) (Selected s (FilterMutable (ModsOf cls cont)))









--type IxedData cls idx = If (IxedMode cls :== Single) idx [idx]

--type family QueryData (info :: *) (query :: [*]) :: * where
--    QueryData info '[]       = ()
--    QueryData info (q ': qs) = (ModData q info, QueryData info qs)

--type family   ModData mod info
--type instance ModData Mods.Ixed (Info idx el cls cont) = IxedData cls (
--    If (idx :== NA) (
--        --If (el :== NA)
--            (IndexOf' cont)
--          --  (IndexOf el cont)
--    ) idx
-- )





--type ComputeSelection (cls :: k) (cont :: *) (q :: [*]) = LstIn (ModsOf cls cont) q
--type AssumeQuery i q s = QueryData (DataStoreInfo i) (FilterMutable q) ~ SelTags i s




--runModsF :: ComputeSelection cls cont q ~ s => (Proxy (q :: [*])) -> (Query q s -> Info idx el cls cont -> sig) -> sig
--runModsF _ f = f Query Info


--runModsF' = flip runModsF



--type family ContainerOf a
--class HasContainer a where
--    container :: Lens' a (ContainerOf a)

--type family DataStoreOf a

--class HasDataStore a where
--    dataStore :: Lens' a (DataStoreOf a)

--class HasDataStore a => IsDataStore a where
--    fromDataStore :: DataStoreOf a -> a



--type family InfoSelection i q where InfoSelection   (Info idx el cls cont) q = ComputeSelection cls cont q
--type family DataStoreInfo i   where DataStoreInfo   (Info idx el cls cont)   = Info idx el cls (DataStoreOf cont)



--type family InfoInst info q m :: Constraint where
--    InfoInst (Info NA  NA cls cont) q m = cls        cont m q (ComputeSelection cls cont q)
--    InfoInst (Info NA  el cls cont) q m = cls     el cont m q (ComputeSelection cls cont q)
--    InfoInst (Info idx NA cls cont) q m = cls idx    cont m q (ComputeSelection cls cont q)
--    InfoInst (Info idx el cls cont) q m = cls idx el cont m q (ComputeSelection cls cont q)






------






--type family Ixed (op :: k) :: k where Ixed (cls q (m ::  * -> *) :: * -> Constraint) = cls (AppendLst Mods.Ixed q) m
--                                      Ixed (cls q    :: (* -> *) -> * -> Constraint) = cls (AppendLst Mods.Ixed q)



--type family Unchecked (op :: k) :: k where Unchecked (cls q (m ::  * -> *) :: * -> Constraint) = cls (AppendLst Mods.Unchecked q) m
--                                           Unchecked (cls q    :: (* -> *) -> * -> Constraint) = cls (AppendLst Mods.Unchecked q)








type family Ixed (op :: k) :: k where
    Ixed (op (ms :: [*]) (ps :: [*]))               = op (Mods.Ixed ': ms) ps
    Ixed (op (ms :: [*]) (ps :: [*]) (m :: * -> *)) = op (Mods.Ixed ': ms) ps m

---- === Concatenation ===








--type family IxedMode (a :: k) :: IxedType

--data IxedType = Multi
--              | Single
--              deriving (Show)



----type family ClassOf (a :: *) :: k

----------------------------------







--newtype NestedFunctor m n a = NestedFunctor { fromNestedFunctor :: m (n a)} deriving (Show)
--instance (Functor m, Functor n) => Functor (NestedFunctor m n) where fmap f = NestedFunctor . (fmap $ fmap f) . fromNestedFunctor


--nested :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
--nested l f = fromNestedFunctor . l (fmap NestedFunctor f)





--data Result d r = Result d r deriving (Show, Functor)

---- Result utils

--simple' = Result ()
--res     = Result . (,())
--resM    = return .: res
--simpleM = return . simple'

--withResData :: (d -> (out, d')) -> Result d r -> (out, Result d' r)
--withResData f (Result d r) = (out, Result d' r) where
--  (out, d') = f d

--withResData_ :: (d -> d') -> Result d r -> Result d' r
--withResData_ = flattenMod withResData

--splitResData :: Result (d,ds) r -> (d, Result ds r)
--splitResData = withResData id

--flattenMod :: Functor f => (f ((), a) -> b -> (x, c)) -> f a -> b -> c
--flattenMod f = snd .: (f . fmap ((),))



----type Unique lst = Reverse (Unique' lst '[])
--type Unique lst = (Unique2' lst)

--type family Unique' (lst :: [*]) (reg :: [*]) where
--  Unique' '[]       reg = reg
--  Unique' (l ': ls) reg = Unique' ls (If (l `In` reg) reg (l ': reg))

--type family Unique2' (lst :: [*]) where
--  Unique2' '[]       = '[]
--  Unique2' (l ': ls) = l ': Unique2' (Remove l ls)




--uniqueProxy :: Proxy a -> Proxy (Unique a)
--uniqueProxy _ = Proxy






--newtype Flipped t a b = Flipped { fromFlipped :: t b a } deriving Show

--instance Functor (Flipped Result r) where fmap f (Flipped (Result d r)) = Flipped (Result (f d) r)


--withFlipped f = fromFlipped . f . Flipped



--foo f (info :: Proxy (cls :: k)) (q :: Proxy (q :: [*])) (tc :: cont) = out where
--    tc2 = runModsF' f (uniqueProxy q) tc
--    tc3 = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection cls cont q) (FilterMutable (ModsOf cls cont))) )) <$> tc2
--    out = tc3

----bar :: OpCtx ExpandableInfo2 q m t => Proxy q -> t -> m (Result (MyResult ExpandableInfo2 q t) t)
--bar f info q t = out where
--    q'    = filterMutable q
--    cont  = view container t
--    tgdr  = foo f info q cont
--    tgdr' = (fmap . fmap) (\c -> t & container .~ c) tgdr
--    out   = withFlipped (fmap (fillData q')) <$> tgdr'


--barTx f (cls :: Proxy (cls :: k)) (q :: Proxy (q :: [*])) (t :: t) = withFlipped (fmap (fillData (filterMutable q))) <$> nested container tgdr t where
--    tgdr c  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection cls (ContainerOf t) q) (FilterMutable (ModsOf cls (ContainerOf t)))) )) <$> f (uniqueProxy q) c


--barTy f (cls :: Proxy (cls :: k)) (q :: Proxy (q :: [*])) (t :: t) = withFlipped (fmap (fillData (filterMutable q))) <$> tgdr (view container t) where
--    tgdr c  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection cls (ContainerOf t) q) (FilterMutable (ModsOf cls (ContainerOf t)))) )) <$> f (uniqueProxy q) c

----bar2z f (cls :: Proxy (cls :: k)) (q :: Proxy (q :: [*])) (t :: t) = withFlipped (fmap (fillData (filterMutable q))) <$> tgdr (view container t) where
----    tgdr c  = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection cls (ContainerOf t) q) (FilterMutable (ModsOf cls (ContainerOf t)))) )) <$> f (uniqueProxy q) c



--type family AssumeRtupConv q t :: Constraint where AssumeRtupConv q t = If' (Mods.FilterMutable q :== '[]) (AsRTup t ~ (t,())) ()

--type family OpCtx info q m t where OpCtx (Info idx el cls) q m t = ((Functor m,
--                                               HasContainer t,
--                                               DataFillable
--                                                 (FilterMutable q)
--                                                 (TaggedCont
--                                                    (Selected
--                                                       (LstIn
--                                                          (ModsOf cls (ContainerOf t)) q)
--                                                       (FilterMutable
--                                                          (ModsOf cls (ContainerOf t))))
--                                                    (QueryData
--                                                       (Info idx el cls (DataStoreOf t))
--                                                       (Selected
--                                                          (LstIn
--                                                             (ModsOf cls (ContainerOf t))
--                                                             (Unique q))
--                                                          (FilterMutable
--                                                             (ModsOf
--                                                                cls (ContainerOf t)))))),
--                                               Taggable
--                                                 (Selected
--                                                    (LstIn
--                                                       (ModsOf cls (ContainerOf t)) q)
--                                                    (FilterMutable
--                                                       (ModsOf cls (ContainerOf t))))
--                                                 (QueryData
--                                                    (Info idx el cls (DataStoreOf t))
--                                                    (Selected
--                                                       (LstIn
--                                                          (ModsOf cls (ContainerOf t))
--                                                          (Unique q))
--                                                       (FilterMutable
--                                                          (ModsOf
--                                                             cls (ContainerOf t))))),
--                                               QueryData
--                                                 (Info idx el cls (DataStoreOf t))
--                                                 (FilterMutable (Unique q))
--                                               ~ QueryData
--                                                   (Info idx el cls (DataStoreOf t))
--                                                   (Selected
--                                                      (LstIn
--                                                         (ModsOf cls (ContainerOf t))
--                                                         (Unique q))
--                                                      (FilterMutable
--                                                         (ModsOf cls (ContainerOf t)))),
--                                               QueryData
--                                                 (Info idx el cls (DataStoreOf t))
--                                                 (FilterMutable q)
--                                               ~ FillData
--                                                   (FilterMutable q)
--                                                   (TaggedCont
--                                                      (Selected
--                                                         (LstIn
--                                                            (ModsOf cls (ContainerOf t))
--                                                            q)
--                                                         (FilterMutable
--                                                            (ModsOf
--                                                               cls (ContainerOf t))))
--                                                      (QueryData
--                                                         (Info idx el cls (DataStoreOf t))
--                                                         (Selected
--                                                            (LstIn
--                                                               (ModsOf
--                                                                  cls (ContainerOf t))
--                                                               (Unique q))
--                                                            (FilterMutable
--                                                               (ModsOf
--                                                                  cls
--                                                                  (ContainerOf t))))))),

--                                                                  -- manual
--                                                                  Functor m,
--                                                                  AssumeRtupConv q t,
--                                                                  IsContainer t,
--                                                                  InfoInst (Info idx el cls (ContainerOf t)) (Unique q) m,
--                                                                  DataStoreOf (ContainerOf t) ~ DataStoreOf t,
--                                                                  (QueryData (Info idx el cls (DataStoreOf t)) (FilterMutable q) ~ QueryData (Info idx el cls (DataStoreOf t)) (FilterMutable q)))


--type family TransCheck q info info' t where
--    TransCheck q (Info idx el cls) info' t = (FillData
--                        (FilterMutable q)
--                        (TaggedCont
--                           (Selected
--                              (LstIn (ModsOf cls (ContainerOf t)) q)
--                              (FilterMutable (ModsOf cls (ContainerOf t))))
--                           (QueryData
--                              (Info idx el cls (DataStoreOf t))
--                              (Selected
--                                 (LstIn
--                                    (ModsOf cls (ContainerOf t))
--                                    (Unique q))
--                                 (FilterMutable (ModsOf cls (ContainerOf t))))))
--                      ~ QueryData
--                          (info' (DataStoreOf t)) (FilterMutable q))






----type family Test (a :: k) where Test a = (If a :== (t1,t2)) True True


----type family Match

--class IsTuple a (is :: Bool) | a -> is

--instance {-# OVERLAPPABLE #-} is ~ False => IsTuple a is
--instance                                    IsTuple () True
--instance                                    IsTuple (t1,t2) True
--instance                                    IsTuple (t1,t2,t3) True
--instance                                    IsTuple (t1,t2,t3,t4) True
--instance                                    IsTuple (t1,t2,t3,t4,t5) True
--instance                                    IsTuple (t1,t2,t3,t4,t5,t6) True
--instance                                    IsTuple (t1,t2,t3,t4,t5,t6,t7) True
--instance                                    IsTuple (t1,t2,t3,t4,t5,t6,t7,t8) True
--instance                                    IsTuple (t1,t2,t3,t4,t5,t6,t7,t8,t9) True

---- FIXME [WD]: below ToTupCtx* refer to need of AssumeRtupConv function, which adds special case context when we are returning only single value from Rtuple (t,())
---- It is not seen properly byGHC though
----type family ToTupCtx a :: Constraint where
----  ToTupCtx (t,()) = (AsRTup t ~ (t,()))
----  ToTupCtx t      = ()

----type family ToTupCtx2 a :: Constraint where
----  ToTupCtx2 (t1,t2) = ()
----  ToTupCtx2 (t1,t2,t3) = ()
----  ToTupCtx2 (t1,t2,t3,t4) = ()
----  ToTupCtx2 (t1,t2,t3,t4,t5) = ()
----  ToTupCtx2 (t1,t2,t3,t4,t5,t6) = ()
----  ToTupCtx2 (t1,t2,t3,t4,t5,t6,t7) = ()
----  ToTupCtx2 (t1,t2,t3,t4,t5,t6,t7,t8) = ()
----  ToTupCtx2 (t1,t2,t3,t4,t5,t6,t7,t8,t9) = ()
----  ToTupCtx2 t = (AsRTup t ~ (t,()))

--  ---- Utils

--type family   AsTup a
--type instance AsTup () = ()
--type instance AsTup (t1,()) = t1
--type instance AsTup (t1,(t2,())) = (t1,t2)
--type instance AsTup (t1,(t2,(t3,()))) = (t1,t2,t3)
--type instance AsTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
--type instance AsTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
--type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
--type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
--type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
--type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)

--type family   AsRTup a where
--    AsRTup () = ()
--    AsRTup (t1,t2) = (t1,(t2,()))
--    AsRTup (t1,t2,t3) = (t1,(t2,(t3,())))
--    AsRTup (t1,t2,t3,t4) = (t1,(t2,(t3,(t4,()))))
--    AsRTup (t1,t2,t3,t4,t5) = (t1,(t2,(t3,(t4,(t5,())))))
--    AsRTup (t1,t2,t3,t4,t5,t6) = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
--    AsRTup (t1,t2,t3,t4,t5,t6,t7) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
--    AsRTup (t1,t2,t3,t4,t5,t6,t7,t8) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
--    AsRTup (t1,t2,t3,t4,t5,t6,t7,t8,t9) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
--    AsRTup a = (a,())


--class    a ~ AsRTup (AsTup a) => ToTup a  where toTup :: a -> AsTup a
--instance ToTup () where toTup _ = ()
--instance AsRTup t1 ~ (t1,()) => ToTup (t1,()) where toTup (t1,()) = t1
--instance ToTup (t1,(t2,())) where toTup (t1,(t2,())) = (t1,t2)
--instance ToTup (t1,(t2,(t3,()))) where toTup (t1,(t2,(t3,()))) = (t1,t2,t3)
--instance ToTup (t1,(t2,(t3,(t4,())))) where toTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
--instance ToTup (t1,(t2,(t3,(t4,(t5,()))))) where toTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
--instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
--instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
--instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
--instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)

--type SimpleRes a = AsRTup a ~ (a, ())








type Simple (t :: [*] -> [*] -> k) = t '[] '[]
