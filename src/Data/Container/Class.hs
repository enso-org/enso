{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE CPP                       #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif


module Data.Container.Class (module Data.Container.Class, module X) where

--import Prologue_old hiding (Indexable, index, Bounded, Ixed, Simple, Indexed)
import Prelude hiding ((.))

import Control.Lens as X (Index)

import qualified Data.Container.Opts    as Opts
import           Data.Container.Opts    (Query(..), Opt(..), Knowledge(..), queryBuilder, withTransFunc)
import           Data.Container.Poly    (Simple)
import           GHC.Exts               (Constraint)
import           Data.Functor.Utils
import           Control.Monad.Identity
import           Control.Lens           hiding (Indexable, index, Bounded, Ixed, Simple, Indexed)
import           Data.List              (intersperse)
import           Data.Typeable

data Impossible    = Impossible  deriving (Show)
data ImpossibleM a = ImpossibleM deriving (Show, Functor)
type ImpTL = '[Impossible]



impossible = error "Impossible happened."


------------------------------------
-- === Data / Component store === --
------------------------------------

-- | Points to the real structure handling the data
type family DataStore a
--class HasDataStore t a where
--    dataStore :: Lens' a (DataStore a)

-- | Points to the real structure handling the data of specific component
type family ComponentStore t a
class HasComponentStore t a where
    componentStore :: Proxy t -> Lens' a (ComponentStore t a)


-------------------------------
-- === General utilities === --
-------------------------------

type family Container a
type family Item      a


--type        Item      a = Item (Index a)


type  HasContainer = HasContainerM Identity
class HasContainerM m a where
    viewContainerM :: a -> m (Container a)
    setContainerM  :: Container a -> a -> m a
    default viewContainerM :: (Container a ~ Container (Unwrapped a), Wrapped a, HasContainerM m (Unwrapped a)) => a -> m (Container a)
    viewContainerM = viewContainerM . view _Wrapped'
    default setContainerM :: (Container a ~ Container (Unwrapped a), Wrapped a, HasContainerM m (Unwrapped a), Functor m) => Container a -> a -> m a
    setContainerM = _Wrapped' . setContainerM

type                    IsContainer  = IsContainerM Identity
class HasContainerM m a => IsContainerM m a where
    fromContainerM :: Container a -> m a
    --default fromContainerM :: (Monad m) => Container a -> m a
    --fromContainerM = view unwrapped' . fromContainerM


container :: HasContainer a => Lens' a (Container a)
container = lens (runIdentity . viewContainerM) (runIdentity .: flip setContainerM)

fromContainer :: IsContainer a => Container a -> a
fromContainer = runIdentity . fromContainerM

withContainerM :: (Monad m, HasContainerM m a) => (Container a -> m (Container a)) -> a -> m a
withContainerM f l = viewContainerM l >>= f >>= flip setContainerM l

withContainerM' :: (Monad m, HasContainerM m a) => (Container a -> Container a) -> a -> m a
withContainerM' = withContainerM . (return .)

-- Type level utilities

type family PrimStoreOf    a where PrimStoreOf    a = PrimStoreOf' (DataStore a) a
type family PrimStoreOf' t a where PrimStoreOf' a a = a
                                   PrimStoreOf' t a = PrimStoreOf' (DataStore (Container (DataStore a))) (Container (DataStore a))


----------------------------
-- === Results & Infos ===
----------------------------

-- === Info ===

data Info idx el cont = Info idx el cont

type PrimInfo         = 'Info 'Unknown     'Unknown
type ElInfo        el = 'Info 'Unknown     ('Known el)
type IdxInfo   idx    = 'Info ('Known idx) 'Unknown
type IdxElInfo idx el = 'Info ('Known idx) ('Known el)



-- === Results ===

data Res datas a = Res datas a deriving (Show, Functor, Traversable, Foldable)

type Result op info mods = Res (Result_ op info mods)

type PrimResult  op ms        a = Result op (PrimInfo         a) ms
type ElResult    op ms     el a = Result op (ElInfo        el a) ms
type IdxResult   op ms idx    a = Result op (IdxInfo   idx    a) ms
type IdxElResult op ms idx el a = Result op (IdxElInfo idx el a) ms

type family Result_ op info (mods :: [*]) where
    Result_ op info '[]       = ()
    Result_ op info (m ': ms) = (ModResult op info m, Result_ op info ms)



-- === Opts ===

type family IdxMod op ix

type family ModResult op (info :: Info (Knowledge *) (Knowledge *) *) mod

type family GetOpts (m :: [Opt *]) :: [*] where
    GetOpts '[] = '[]
    GetOpts (P a ': ms) = a ': GetOpts ms
    GetOpts (N   ': ms) = GetOpts ms



-- === >>> Mods ===

type instance ModResult op ('Info (Known idx) el cont) Opts.Ixed = IdxMod op idx
type instance ModResult op ('Info Unknown     el cont) Opts.Ixed = IdxMod op (Index cont)


-----------------------------
-- === Operations classes ===
-----------------------------

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class Ctx ms m cont => MeasurableQM ms ps m     cont where sizeQM     :: Query ms ps -> cont -> m (PrimResult MeasurableOp ms     (Container cont) Int)
class Ctx ms m cont => MinBoundedQM ms ps m idx cont where minBoundQM :: Query ms ps -> cont -> m (IdxResult  MinBoundedOp ms idx (Container cont) idx)
class Ctx ms m cont => MaxBoundedQM ms ps m idx cont where maxBoundQM :: Query ms ps -> cont -> m (IdxResult  MaxBoundedOp ms idx (Container cont) idx)

data MeasurableOp      = MeasurableOp
type MeasurableM       = Simple MeasurableQM
type MeasurableQ ms ps = MeasurableQM ms ps Identity
type Measurable        = Simple MeasurableQM Identity

data MinBoundedOp      = MinBoundedOp
type MinBoundedM       = Simple MinBoundedQM
type MinBoundedQ ms ps = MinBoundedQM ms ps Identity
type MinBounded        = Simple MinBoundedQM Identity

data MaxBoundedOp      = MaxBoundedOp
type MaxBoundedM       = Simple MaxBoundedQM
type MaxBoundedQ ms ps = MaxBoundedQM ms ps Identity
type MaxBounded        = Simple MaxBoundedQM Identity

type BoundedQM ms ps m idx cont = (MinBoundedQM ms ps m idx cont, MaxBoundedQM ms ps m idx cont)
type BoundedM        m idx cont = Simple MaxBoundedQM m idx cont
type BoundedQ  ms ps m idx cont = MaxBoundedQM ms ps Identity idx cont
type Bounded           idx cont = BoundedM Identity idx cont

-- utils

sizeM'     = queryBuilder sizeQM
size'      = withTransFunc (fmap2 runIdentity) sizeM'
sizeM      = queryBuilder $ fmap formatResult .: sizeQM
size       = withTransFunc (fmap2 runIdentity) sizeM

minBoundM' = queryBuilder minBoundQM
minBound'  = withTransFunc (fmap2 runIdentity) minBoundM'
minBoundM  = queryBuilder $ fmap formatResult .: minBoundQM
minBound   = withTransFunc (fmap2 runIdentity) minBoundM

maxBoundM' = queryBuilder maxBoundQM
maxBound'  = withTransFunc (fmap2 runIdentity) maxBoundM'
maxBoundM  = queryBuilder $ fmap formatResult .: maxBoundQM
maxBound   = withTransFunc (fmap2 runIdentity) maxBoundM




-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable

class Ctx ms m cont => SingletonQM  ms ps m el cont where singletonQM :: Query ms ps         -> el   -> m (ElResult   SingletonOp  ms el (Container cont) cont)
class Ctx ms m cont => ExpandableQM ms ps m    cont where expandQM    :: Query ms ps         -> cont -> m (PrimResult ExpandableOp ms    (Container cont) cont)
class Ctx ms m cont => AllocableQM  ms ps m    cont where allocQM     :: Query ms ps -> Int          -> m (PrimResult AllocableOp  ms    (Container cont) cont)
class Ctx ms m cont => GrowableQM   ms ps m    cont where growQM      :: Query ms ps -> Int  -> cont -> m (PrimResult GrowableOp   ms    (Container cont) cont)

data  SingletonOp       = SingletonOp
type  SingletonM        = Simple SingletonQM
type  SingletonQ  ms ps = SingletonQM ms ps Identity
type  Singleton         = Simple SingletonQM Identity

data  AllocableOp       = AllocableOp
type  AllocableM        = Simple AllocableQM
type  AllocableQ ms ps  = AllocableQM ms ps Identity
type  Allocable         = Simple AllocableQM Identity

data  ExpandableOp      = ExpandableOp
type  ExpandableM       = Simple ExpandableQM
type  ExpandableQ ms ps = ExpandableQM ms ps Identity
type  Expandable        = Simple ExpandableQM Identity

data  GrowableOp        = GrowableOp
type  GrowableM         = Simple GrowableQM
type  GrowableQ ms ps   = GrowableQM ms ps Identity
type  Growable          = Simple GrowableQM Identity

type instance IdxMod SingletonOp  a = a
type instance IdxMod AllocableOp  a = [a]
type instance IdxMod ExpandableOp a = [a]
type instance IdxMod GrowableOp   a = [a]

-- utils

singletonM' = queryBuilder singletonQM
singleton'  = withTransFunc (fmap2 runIdentity) singletonM'
singletonM  = queryBuilder $ fmap formatResult .: singletonQM
singleton   = withTransFunc (fmap2 runIdentity) singletonM

allocM'     = queryBuilder allocQM
alloc'      = withTransFunc (fmap2 runIdentity) allocM'
allocM      = queryBuilder $ fmap formatResult .: allocQM
alloc       = withTransFunc (fmap2 runIdentity) allocM

expandM'    = queryBuilder expandQM
expand'     = withTransFunc (fmap2 runIdentity) expandM'
expandM     = queryBuilder $ fmap formatResult .: expandQM
expand      = withTransFunc (fmap2 runIdentity) expandM

growM'      = queryBuilder growQM
grow'       = withTransFunc (fmap3 runIdentity) growM'
growM       = queryBuilder $ fmap formatResult .:. growQM
grow        = withTransFunc (fmap3 runIdentity) growM



-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable
-- Insertable
-- Freeable


-- TODO[WD]: New interface. All metadata should be registered in a Result monad.
--           We should be able to define either an AddableM instance directly, which regardless of the mods and opts would have the same constraint, something like:
--
--               class AddableM mods opts el m cont where addM :: el -> cont -> m (Result (ElInfo el) mods opts cont)
--
--               instance (a ~ el, Provided mods Idx Int) => AddableM mods opts el m (Vector a) where
--                   addM el v = do
--                       provide Idx ...
--                       return ...
--
--           We should be also able to use the mods sorting mechanism like the current solution, but choosing manually which instances need it.
--           Additional we should reconsider how we decide modify result arguments based on the function type, for example the `alloc` function used with `ixed` modifier results in a list of ixes, not a single value
--           One more thing to consider are the functions that change the type of the container like `thaw` or `freeze` (like the ones in Data.Vector package)

class Ctx ms m cont => AppendableQM   ms ps m     el cont where appendQM  :: Query ms ps        -> el -> cont -> m (ElResult    AppendableOp    ms     el (Container cont) cont)
class Ctx ms m cont => PrependableQM  ms ps m     el cont where prependQM :: Query ms ps        -> el -> cont -> m (ElResult    PrependableOp   ms     el (Container cont) cont)
class Ctx ms m cont => AddableQM      ms ps m     el cont where addQM     :: Query ms ps        -> el -> cont -> m (ElResult    AddableOp       ms     el (Container cont) cont)
class Ctx ms m cont => RemovableQM    ms ps m     el cont where removeQM  :: Query ms ps        -> el -> cont -> m (ElResult    RemovableOp     ms     el (Container cont) cont)
class Ctx ms m cont => InsertableQM   ms ps m idx el cont where insertQM  :: Query ms ps -> idx -> el -> cont -> m (IdxElResult InsertableOp    ms idx el (Container cont) cont)
class Ctx ms m cont => FreeableQM     ms ps m idx    cont where freeQM    :: Query ms ps -> idx       -> cont -> m (IdxResult   FreeableOp      ms idx    (Container cont) cont)
class Ctx ms m cont => ReservableQM   ms ps m        cont where reserveQM :: Query ms ps              -> cont -> m (PrimResult  ReservableOp    ms        (Container cont) cont)



data  AppendableOp       = AppendableOp
type  AppendableM        = Simple AppendableQM
type  AppendableQ  ms ps = AppendableQM ms ps Identity
type  Appendable         = Simple AppendableQM Identity

data  PrependableOp      = PrependableOp
type  PrependableM       = Simple PrependableQM
type  PrependableQ ms ps = PrependableQM ms ps Identity
type  Prependable        = Simple PrependableQM Identity

data  AddableOp          = AddableOp
type  AddableM           = Simple AddableQM
type  AddableQ     ms ps = AddableQM ms ps Identity
type  Addable            = Simple AddableQM Identity

data  RemovableOp        = RemovableOp
type  RemovableM         = Simple RemovableQM
type  RemovableQ   ms ps = RemovableQM ms ps Identity
type  Removable          = Simple RemovableQM Identity

data  InsertableOp       = InsertableOp
type  InsertableM        = Simple InsertableQM
type  InsertableQ  ms ps = InsertableQM ms ps Identity
type  Insertable         = Simple InsertableQM Identity

data  FreeableOp         = FreeableOp
type  FreeableM          = Simple FreeableQM
type  FreeableQ    ms ps = FreeableQM ms ps Identity
type  Freeable           = Simple FreeableQM Identity

data  ReservableOp       = ReservableOp
type  ReservableM        = Simple ReservableQM
type  ReservableQ  ms ps = ReservableQM ms ps Identity
type  Reservable         = Simple ReservableQM Identity

type instance IdxMod AppendableOp  a = a
type instance IdxMod PrependableOp a = a
type instance IdxMod AddableOp     a = a
type instance IdxMod RemovableOp   a = a
type instance IdxMod InsertableOp  a = a
type instance IdxMod ReservableOp  a = a

appendM'  = queryBuilder appendQM
append'   = withTransFunc (fmap3 runIdentity) appendM'
appendM   = queryBuilder $ fmap formatResult .:. appendQM
append    = withTransFunc (fmap3 runIdentity) appendM
{-# INLINE appendM' #-}
{-# INLINE append'  #-}
{-# INLINE appendM  #-}
{-# INLINE append   #-}

prependM' = queryBuilder prependQM
prepend'  = withTransFunc (fmap3 runIdentity) prependM'
prependM  = queryBuilder $ fmap formatResult .:. prependQM
prepend   = withTransFunc (fmap3 runIdentity) prependM

addM'     = queryBuilder addQM
add'      = withTransFunc (fmap3 runIdentity) addM'
rawAdd    = withTransFunc (fmap3 resToRTup) add' -- FIXME[WD] - we should remove Res datatype and implement it just as 2 elems tuple. This will allow us to create prefix "raw" which will reuslt just in tuple.
addM      = queryBuilder $ fmap formatResult .:. addQM
add       = withTransFunc (fmap3 runIdentity) addM
add_      = withTransFunc (fmap3 res_) add'

removeM'  = queryBuilder removeQM
remove'   = withTransFunc (fmap3 runIdentity) removeM'
removeM   = queryBuilder $ fmap formatResult .:. removeQM
remove    = withTransFunc (fmap3 runIdentity) removeM

insertM'  = queryBuilder insertQM
insert'   = withTransFunc (fmap4 runIdentity) insertM'
insertM   = queryBuilder $ fmap formatResult .:: insertQM
insertM__    = withTransFunc (fmap5 res_) insertM'
insert    = withTransFunc (fmap4 runIdentity) insertM
insert_   = withTransFunc (fmap4 res_) insert'
{-# INLINE insertM' #-}
{-# INLINE insert'  #-}
{-# INLINE insertM  #-}
{-# INLINE insert   #-}
{-# INLINE insert_  #-}

freeM'    = queryBuilder freeQM
free'     = withTransFunc (fmap3 runIdentity) freeM'
freeM     = queryBuilder $ fmap formatResult .:. freeQM
free      = withTransFunc (fmap3 runIdentity) freeM
free_     = withTransFunc (fmap3 res_) free'

reserveM' = queryBuilder reserveQM
reserve'  = withTransFunc (fmap2 runIdentity) reserveM'
reserveM  = queryBuilder $ fmap formatResult .: reserveQM
reserve   = withTransFunc (fmap2 runIdentity) reserveM



---- === Indexing ===

-- Indexable
-- TracksFreeIxes
-- TracksUsedIxes
-- TracksIxes
-- TracksElems

class Ctx ms m cont => IndexableQM      ms ps m idx el cont where indexQM    :: Query ms ps -> idx -> cont -> m (IdxElResult IndexableOp      ms idx el (Container cont) el   )
class Ctx ms m cont => TracksFreeIxesQM ms ps m idx    cont where freeIxesQM :: Query ms ps ->        cont -> m (IdxResult   TracksFreeIxesOp ms idx    (Container cont) [idx])
class Ctx ms m cont => TracksUsedIxesQM ms ps m idx    cont where usedIxesQM :: Query ms ps ->        cont -> m (IdxResult   TracksUsedIxesOp ms idx    (Container cont) [idx])
class Ctx ms m cont => TracksIxesQM     ms ps m idx    cont where ixesQM     :: Query ms ps ->        cont -> m (IdxResult   TracksIxesOp     ms idx    (Container cont) [idx])
class Ctx2 m => TracksElemsQM    ms ps m     el cont where elemsQM    :: Query ms ps ->        cont -> m (ElResult    TracksElemsOp    ms     el (Container cont) [el] )

data  IndexableOp            = IndexableOp
type  IndexableM             = Simple IndexableQM
type  IndexableQ       ms ps = IndexableQM ms ps Identity
type  Indexable              = Simple IndexableQM Identity

data  TracksFreeIxesOp       = TracksFreeIxesOp
type  TracksFreeIxesM        = Simple TracksFreeIxesQM
type  TracksFreeIxesQ  ms ps = TracksFreeIxesQM ms ps Identity
type  TracksFreeIxes         = Simple TracksFreeIxesQM Identity

data  TracksUsedIxesOp       = TracksUsedIxesOp
type  TracksUsedIxesM        = Simple TracksUsedIxesQM
type  TracksUsedIxesQ  ms ps = TracksUsedIxesQM ms ps Identity
type  TracksUsedIxes         = Simple TracksUsedIxesQM Identity

data  TracksIxesOp           = TracksIxesOp
type  TracksIxesM            = Simple TracksIxesQM
type  TracksIxesQ      ms ps = TracksIxesQM ms ps Identity
type  TracksIxes             = Simple TracksIxesQM Identity

data  TracksElemsOp          = TracksElemsOp
type  TracksElemsM           = Simple TracksElemsQM
type  TracksElemsQ     ms ps = TracksElemsQM ms ps Identity
type  TracksElems            = Simple TracksElemsQM Identity

type instance IdxMod IndexableOp   a = a
type instance IdxMod TracksElemsOp a = [a]

indexM'    = queryBuilder indexQM
index'     = withTransFunc (fmap3 runIdentity) indexM'
indexM     = queryBuilder $ fmap formatResult .:. indexQM
indexM__    = withTransFunc (fmap4 res_) indexM'
index      = withTransFunc (fmap3 runIdentity) indexM
index_     = withTransFunc (fmap3 res_) index'

freeIxesM' = queryBuilder freeIxesQM
freeIxes'  = withTransFunc (fmap2 runIdentity) freeIxesM'
freeIxesM  = queryBuilder $ fmap formatResult .: freeIxesQM
freeIxes   = withTransFunc (fmap2 runIdentity) freeIxesM

usedIxesM' = queryBuilder usedIxesQM
usedIxes'  = withTransFunc (fmap2 runIdentity) usedIxesM'
usedIxesM  = queryBuilder $ fmap formatResult .: usedIxesQM
usedIxes   = withTransFunc (fmap2 runIdentity) usedIxesM
usedIxes_  = withTransFunc (fmap2 res_) usedIxes'

ixesM'     = queryBuilder ixesQM
ixes'      = withTransFunc (fmap2 runIdentity) ixesM'
ixesM      = queryBuilder $ fmap formatResult .: ixesQM
ixes       = withTransFunc (fmap2 runIdentity) ixesM

elemsM'    = queryBuilder elemsQM
elems'     = withTransFunc (fmap2 runIdentity) elemsM'
elemsM     = queryBuilder $ fmap formatResult .: elemsQM
elems      = withTransFunc (fmap2 runIdentity) elemsM
elems_     = withTransFunc (fmap2 res_) elems'






type family Tup2RTup t where
    Tup2RTup ()               = ()
    Tup2RTup (t1, t2)         = (t1,(t2,()))
    Tup2RTup (t1, t2, t3)     = (t1,(t2,(t3,())))
    Tup2RTup (t1, t2, t3, t4) = (t1,(t2,(t3,(t4,()))))
    Tup2RTup a                = (a,())

type family AppendedRT a rt where
    AppendedRT a ()     = (a,())
    AppendedRT a (r,rt) = (r, AppendedRT a rt)

--class    AppendRT a rt                      where appendRT :: a -> rt -> AppendedRT a rt
--instance AppendRT a ()                      where appendRT a _      = (a,())
--instance AppendRT a rt => AppendRT a (r,rt) where appendRT a (r,rt) = (r,appendRT a rt)

--class                         AppendRT a rt rt' | a rt -> rt', rt' -> rt a where appendRT :: a -> rt -> rt'
--instance                      AppendRT a ()     (a,())                     where appendRT a _      = (a,())
--instance AppendRT a rt rt' => AppendRT a (r,rt) (r,rt')                    where appendRT a (r,rt) = (r,appendRT a rt)

type family InitRT rt where
    InitRT (t,()) = ()
    InitRT (t,ts) = (t,InitRT ts)

instance ( AppendRT a rt rt'
         , InitRT (r, rt') ~ (r, rt)
         )               => AppendRT a (r,rt) (r,rt')                 where appendRT a (r,rt) = (r,appendRT a rt)
instance                    AppendRT a ()     (a,())                  where appendRT a _      = (a,())
class    rt ~ InitRT rt' => AppendRT a rt rt' | a rt -> rt', rt' -> a where appendRT :: a -> rt -> rt'

resToRTup (Res ds a) = (a,ds)
res_      (Res _  a) = a
{-# INLINE resToRTup #-}
{-# INLINE res_      #-}

--resToRTup (Res ds a) = appendRT a ds

formatResult = rtup2tupX . resToRTup
{-# INLINE formatResult #-}

class rt ~ Tup2RTup t => RTup2TupX rt t | rt -> t where rtup2tupX :: rt -> t
instance {-# OVERLAPPABLE #-}                                      RTup2TupX () () where rtup2tupX = id ; {-# INLINE rtup2tupX #-}
instance {-# OVERLAPPABLE #-} (Tup2RTup t1 ~ (t1,()), t1 ~ t1') => RTup2TupX (t1,()) t1' where rtup2tupX (t1,()) = t1 ; {-# INLINE rtup2tupX #-}
instance {-# OVERLAPPABLE #-} (t1 ~ t1', t2 ~ t2')              => RTup2TupX (t1,(t2,())) (t1',t2') where rtup2tupX (t1,(t2,())) = (t1,t2) ; {-# INLINE rtup2tupX #-}
instance {-# OVERLAPPABLE #-} (t1 ~ t1', t2 ~ t2', t3 ~ t3')    => RTup2TupX (t1,(t2,(t3,()))) (t1',t2',t3') where rtup2tupX (t1,(t2,(t3,()))) = (t1,t2,t3) ; {-# INLINE rtup2tupX #-}



type family PrettyCtx ms a :: Constraint where
    PrettyCtx '[] a = Tup2RTup a ~ (a,())
    PrettyCtx ms  a = ()

type Ctx ms m cont = (Monad m, PrettyCtx ms cont)
type Ctx2 m = (Monad m)









-- Other utils, probably to generalize or refactor

intercalate :: Monoid a => a -> [a] -> a
intercalate delim l = mconcat (intersperse delim l)
{-# INLINE intercalate #-}
