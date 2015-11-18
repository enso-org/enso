{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}


-- | This module provides proxy utilities for Data.Container.Class.
-- | The proxy classes allow for nice options-dependent instance implementations,
-- | hiding at the same time the complexity needed for type-level constrains resolution.

module Data.Container.Proxy where

import Prelude

import Data.Container.Class
import Data.Container.Opts  (GetQueryData, QueryData, ParamsOf, ModsOf, MatchOpts, Knowledge(..), OptQuery(..), Query(..), getQueryData, Opt)
import Data.Typeable        (Proxy(..))
import GHC.Exts             (Constraint)

-----------------------------
-- === Proxy utilities === --
-----------------------------

-- header utils


type family ConstrainCls op (ms :: [Opt *]) (ps :: [Opt *]) (info :: * -> Info (Knowledge *) (Knowledge *) *) (m :: * -> *) :: * -> Constraint

type OpAxioms     op info ms t = (ResultAxioms op info ms t, Functor (PrimResult op (GetOpts ms) t))
type ResultAxioms op info ms t = Result_ op (info t) (GetOpts ms) ~ Result_ op (info (DataStore t)) (GetOpts ms)

#define OpCtx(cls_, info_) {- Inputs (we need to repeat them, cause OSX cpp preprocessor expands the first occurrence only) -} \
                                 cls      ~ (cls_)                                                                             \
                               , info     ~ (info_)                                                                            \
                           {- derives: -}                                                                                      \
                               , fullInfo ~ info cont                                                                          \
                               , matchMs  ~ MatchOpts (ModsOf   cls cont) ms                                                   \
                               , matchPs  ~ MatchOpts (ParamsOf cls cont) ps                                                   \
                               , provided ~ GetOpts matchMs                                                                    \
                               , opts     ~ Result_ cls fullInfo provided                                                      \
                               , cont     ~ Container a                                                                      \
                               , Monad m                                                                                       \
                           {- Super-class constraints: -}                                                                      \
                               , ConstrainCls cls_ matchMs matchPs info m cont                                                 \
                           {- Data queries: -}                                                                                 \
                               , QueryData    provided ms opts ~ Result_ cls fullInfo ms                                       \
                               , GetQueryData provided ms opts                                                                 \
                           {- Query-selected result equality: -}                                                               \
                               , ResultAxioms cls info matchMs cont                                                            \
                           {- Type prettify-}                                                                                  \
                               , PrettyCtx ms a



-------------------------------------
-- === Proxy operation classes === --
-------------------------------------


    
runOp (Proxy :: Proxy cls) (Proxy :: Proxy cont) f getter setter (Query :: Query ms ps) x = do 
    Res datas c <- f (OptQuery :: OptQuery (MatchOpts (ModsOf cls cont) ms) (MatchOpts (ParamsOf cls cont) ps)) =<< getter x
    Res (getQueryData (Proxy :: Proxy (GetOpts (MatchOpts (ModsOf cls cont) ms))) (Proxy :: Proxy ms) datas) <$> setter c x

#define RUNOP() runOp (Proxy :: Proxy cls) (Proxy :: Proxy cont)

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class Monad m => MeasurableQM_ ms ps m     cont where sizeM_     :: OpAxioms MeasurableOp PrimInfo      ms cont => OptQuery ms ps -> cont -> m (PrimResult MeasurableOp (GetOpts ms)     cont Int)
class Monad m => MinBoundedQM_ ms ps m idx cont where minBoundM_ :: OpAxioms MinBoundedOp (IdxInfo idx) ms cont => OptQuery ms ps -> cont -> m (IdxResult  MinBoundedOp (GetOpts ms) idx cont idx)
class Monad m => MaxBoundedQM_ ms ps m idx cont where maxBoundM_ :: OpAxioms MaxBoundedOp (IdxInfo idx) ms cont => OptQuery ms ps -> cont -> m (IdxResult  MaxBoundedOp (GetOpts ms) idx cont idx)

type instance ConstrainCls MeasurableOp ms ps info                     m = MeasurableQM_ ms ps m
type instance ConstrainCls MinBoundedOp ms ps ('Info (Known idx) el) m = MinBoundedQM_ ms ps m idx
type instance ConstrainCls MaxBoundedOp ms ps ('Info (Known idx) el) m = MaxBoundedQM_ ms ps m idx

instance {-# OVERLAPPABLE #-} Monad m                                              => MeasurableQM ImpTL ps m     a          where sizeQM     = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                   => MeasurableQM ms    ps m     Impossible where sizeQM     = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(MeasurableOp,PrimInfo))    => MeasurableQM ms    ps m     a          where sizeQM     = RUNOP() sizeM_ viewContainerM (const . return)

instance {-# OVERLAPPABLE #-} Monad m                                              => MinBoundedQM ImpTL ps m idx a          where minBoundQM = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                   => MinBoundedQM ms    ps m idx Impossible where minBoundQM = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(MinBoundedOp,IdxInfo idx)) => MinBoundedQM ms    ps m idx a          where minBoundQM = RUNOP() minBoundM_ viewContainerM (const . return)

instance {-# OVERLAPPABLE #-} Monad m                                              => MaxBoundedQM ImpTL ps m idx a          where maxBoundQM = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                   => MaxBoundedQM ms    ps m idx Impossible where maxBoundQM = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(MaxBoundedOp,IdxInfo idx)) => MaxBoundedQM ms    ps m idx a          where maxBoundQM = RUNOP() maxBoundM_ viewContainerM (const . return)
    

-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable


class Monad m => SingletonQM_  ms ps m el cont where singletonM_ :: OpAxioms SingletonOp (ElInfo el) ms cont => OptQuery ms ps         -> el   -> m (ElResult   SingletonOp  (GetOpts ms) el cont cont)
class Monad m => AllocableQM_  ms ps m    cont where allocM_     :: OpAxioms AllocableOp  PrimInfo   ms cont => OptQuery ms ps -> Int          -> m (PrimResult AllocableOp  (GetOpts ms)    cont cont)
class Monad m => ExpandableQM_ ms ps m    cont where expandM_    :: OpAxioms ExpandableOp PrimInfo   ms cont => OptQuery ms ps         -> cont -> m (PrimResult ExpandableOp (GetOpts ms)    cont cont)
class Monad m => GrowableQM_   ms ps m    cont where growM_      :: OpAxioms GrowableOp   PrimInfo   ms cont => OptQuery ms ps -> Int  -> cont -> m (PrimResult GrowableOp   (GetOpts ms)    cont cont)

type instance ConstrainCls SingletonOp  ms ps ('Info idx (Known el)) m = SingletonQM_  ms ps m el
type instance ConstrainCls AllocableOp  ms ps info                     m = AllocableQM_  ms ps m
type instance ConstrainCls ExpandableOp ms ps info                     m = ExpandableQM_ ms ps m
type instance ConstrainCls GrowableOp   ms ps info                     m = GrowableQM_   ms ps m

instance {-# OVERLAPPABLE #-} Monad m                                            => SingletonQM ImpTL  ps m el a          where singletonQM = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                 => SingletonQM ms     ps m el Impossible where singletonQM = impossible
instance {-# OVERLAPPABLE #-} (IsContainerM m a, OpCtx(SingletonOp,ElInfo el))   => SingletonQM ms     ps m el a          where singletonQM = RUNOP() singletonM_ return (const . fromContainerM)

instance {-# OVERLAPPABLE #-} Monad m                                            => AllocableQM ImpTL  ps m    a          where allocQM     = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                 => AllocableQM ms     ps m    Impossible where allocQM     = impossible
instance {-# OVERLAPPABLE #-} (IsContainerM m a, OpCtx(AllocableOp,PrimInfo) )   => AllocableQM ms     ps m    a          where allocQM     = RUNOP() allocM_     return (const . fromContainerM)

instance {-# OVERLAPPABLE #-} Monad m                                            => ExpandableQM ImpTL ps m    a          where expandQM    = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                 => ExpandableQM ms    ps m    Impossible where expandQM    = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(ExpandableOp,PrimInfo))  => ExpandableQM ms    ps m    a          where expandQM    = RUNOP() expandM_ viewContainerM setContainerM

instance {-# OVERLAPPABLE #-} Monad m                                            => GrowableQM ImpTL ps m      a          where growQM      = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                 => GrowableQM ms    ps m      Impossible where growQM      = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(GrowableOp,PrimInfo)  )  => GrowableQM ms    ps m      a          where growQM  q i = RUNOP() (flip growM_ i) viewContainerM setContainerM q


-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable
-- Insertable
-- Freeable
-- Reservable


class Monad m => AppendableQM_  ms ps m     el cont where appendM_  :: OpAxioms AppendableOp  (ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (ElResult    AppendableOp  (GetOpts ms)     el cont cont)
class Monad m => PrependableQM_ ms ps m     el cont where prependM_ :: OpAxioms PrependableOp (ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (ElResult    PrependableOp (GetOpts ms)     el cont cont)
class Monad m => AddableQM_     ms ps m     el cont where addM_     :: OpAxioms AddableOp     (ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (ElResult    AddableOp     (GetOpts ms)     el cont cont)
class Monad m => RemovableQM_   ms ps m     el cont where removeM_  :: OpAxioms RemovableOp   (ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (ElResult    RemovableOp   (GetOpts ms)     el cont cont)
class Monad m => InsertableQM_  ms ps m idx el cont where insertM_  :: OpAxioms InsertableOp  (IdxElInfo idx el) ms cont => OptQuery ms ps -> idx -> el -> cont -> m (IdxElResult InsertableOp  (GetOpts ms) idx el cont cont)
class Monad m => FreeableQM_    ms ps m idx    cont where freeM_    :: OpAxioms FreeableOp    (IdxInfo   idx   ) ms cont => OptQuery ms ps -> idx       -> cont -> m (IdxResult   FreeableOp    (GetOpts ms) idx    cont cont)
class Monad m => ReservableQM_  ms ps m        cont where reserveM_ :: OpAxioms ReservableOp  PrimInfo           ms cont => OptQuery ms ps              -> cont -> m (PrimResult  ReservableOp  (GetOpts ms)        cont cont)

type instance ConstrainCls AppendableOp  ms ps ('Info idx          ('Known el)) m = AppendableQM_  ms ps m     el
type instance ConstrainCls PrependableOp ms ps ('Info idx          ('Known el)) m = PrependableQM_ ms ps m     el
type instance ConstrainCls AddableOp     ms ps ('Info idx          ('Known el)) m = AddableQM_     ms ps m     el
type instance ConstrainCls RemovableOp   ms ps ('Info idx          ('Known el)) m = RemovableQM_   ms ps m     el
type instance ConstrainCls InsertableOp  ms ps ('Info ('Known idx) ('Known el)) m = InsertableQM_  ms ps m idx el
type instance ConstrainCls FreeableOp    ms ps ('Info ('Known idx) el         ) m = FreeableQM_    ms ps m idx
type instance ConstrainCls ReservableOp  ms ps info                             m = ReservableQM_  ms ps m

instance {-# OVERLAPPABLE #-} Monad m                                                   => AppendableQM   ImpTL ps m     el a          where appendQM           = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => AppendableQM   ms    ps m     el Impossible where appendQM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(AppendableOp,ElInfo el))        => AppendableQM   ms    ps m     el a          where appendQM  q     el = RUNOP() (flip appendM_ el) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => PrependableQM  ImpTL ps m     el a          where prependQM          = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => PrependableQM  ms    ps m     el Impossible where prependQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(PrependableOp,ElInfo el))       => PrependableQM  ms    ps m     el a          where prependQM q     el = RUNOP() (flip prependM_ el) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => AddableQM      ImpTL ps m     el a          where addQM              = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => AddableQM      ms    ps m     el Impossible where addQM              = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(AddableOp,ElInfo el))           => AddableQM      ms    ps m     el a          where addQM     q     el = RUNOP() (flip addM_ el) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => RemovableQM    ImpTL ps m     el a          where removeQM           = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => RemovableQM    ms    ps m     el Impossible where removeQM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(RemovableOp,ElInfo el))         => RemovableQM    ms    ps m     el a          where removeQM  q     el = RUNOP() (flip removeM_ el) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => InsertableQM   ImpTL ps m idx el a          where insertQM           = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => InsertableQM   ms    ps m idx el Impossible where insertQM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(InsertableOp,IdxElInfo idx el)) => InsertableQM   ms    ps m idx el a          where insertQM  q idx el = RUNOP() (flip (flip insertM_ idx) el) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => FreeableQM     ImpTL ps m idx    a          where freeQM             = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => FreeableQM     ms    ps m idx    Impossible where freeQM             = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(FreeableOp,IdxInfo idx))        => FreeableQM     ms    ps m idx    a          where freeQM    q idx    = RUNOP() (flip freeM_ idx) viewContainerM setContainerM q

instance {-# OVERLAPPABLE #-} Monad m                                                   => ReservableQM   ImpTL ps m        a          where reserveQM          = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                        => ReservableQM   ms    ps m        Impossible where reserveQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(ReservableOp,PrimInfo))         => ReservableQM   ms    ps m        a          where reserveQM q        = RUNOP() reserveM_ viewContainerM setContainerM q




-- === Indexing ===

-- Indexable
-- TracksFreeIxes
-- TracksUsedIxes
-- TracksIxes
-- TracksElems

class Monad m => IndexableQM_       ms ps m idx el cont where indexM_    :: OpAxioms IndexableOp      (IdxElInfo idx el) ms cont => OptQuery ms ps -> idx -> cont -> m (IdxElResult IndexableOp      (GetOpts ms) idx el cont el   )
class Monad m => TracksFreeIxesQM_  ms ps m idx    cont where freeIxesM_ :: OpAxioms TracksFreeIxesOp (IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (IdxResult   TracksFreeIxesOp (GetOpts ms) idx    cont [idx])
class Monad m => TracksUsedIxesQM_  ms ps m idx    cont where usedIxesM_ :: OpAxioms TracksUsedIxesOp (IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (IdxResult   TracksUsedIxesOp (GetOpts ms) idx    cont [idx])
class Monad m => TracksIxesQM_      ms ps m idx    cont where ixesM_     :: OpAxioms TracksIxesOp     (IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (IdxResult   TracksIxesOp     (GetOpts ms) idx    cont [idx])
class Monad m => TracksElemsQM_     ms ps m     el cont where elemsM_    :: OpAxioms TracksElemsOp    (ElInfo        el) ms cont => OptQuery ms ps        -> cont -> m (ElResult    TracksElemsOp    (GetOpts ms)     el cont [el] )

type instance ConstrainCls IndexableOp      ms ps ('Info ('Known idx) ('Known el)) m = IndexableQM_      ms ps m idx el
type instance ConstrainCls TracksFreeIxesOp ms ps ('Info ('Known idx) el         ) m = TracksFreeIxesQM_ ms ps m idx
type instance ConstrainCls TracksUsedIxesOp ms ps ('Info ('Known idx) el         ) m = TracksUsedIxesQM_ ms ps m idx
type instance ConstrainCls TracksIxesOp     ms ps ('Info ('Known idx) el         ) m = TracksIxesQM_     ms ps m idx
type instance ConstrainCls TracksElemsOp    ms ps ('Info idx          ('Known el)) m = TracksElemsQM_    ms ps m     el

instance {-# OVERLAPPABLE #-} Monad m                                                  => IndexableQM      ImpTL ps m idx el a          where indexQM          = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                       => IndexableQM      ms    ps m idx el Impossible where indexQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(IndexableOp,IdxElInfo idx el)) => IndexableQM      ms    ps m idx el a          where indexQM    q idx = RUNOP() (flip indexM_ idx) viewContainerM (const . return) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => TracksFreeIxesQM ImpTL ps m idx    a          where freeIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                       => TracksFreeIxesQM ms    ps m idx    Impossible where freeIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(TracksFreeIxesOp,IdxInfo idx)) => TracksFreeIxesQM ms    ps m idx    a          where freeIxesQM       = RUNOP() freeIxesM_ viewContainerM (const . return)

instance {-# OVERLAPPABLE #-} Monad m                                                  => TracksUsedIxesQM ImpTL ps m idx    a          where usedIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                       => TracksUsedIxesQM ms    ps m idx    Impossible where usedIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(TracksUsedIxesOp,IdxInfo idx)) => TracksUsedIxesQM ms    ps m idx    a          where usedIxesQM       = RUNOP() usedIxesM_ viewContainerM (const . return)

instance {-# OVERLAPPABLE #-} Monad m                                                  => TracksIxesQM     ImpTL ps m idx    a          where ixesQM           = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                       => TracksIxesQM     ms    ps m idx    Impossible where ixesQM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(TracksIxesOp,IdxInfo idx))     => TracksIxesQM     ms    ps m idx    a          where ixesQM           = RUNOP() ixesM_ viewContainerM (const . return)

instance {-# OVERLAPPABLE #-} Monad m                                                  => TracksElemsQM    ImpTL ps m     el a          where elemsQM          = impossible
instance {-# OVERLAPPABLE #-} (Monad m, PrettyCtx ms Impossible)                       => TracksElemsQM    ms    ps m     el Impossible where elemsQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainerM m a, OpCtx(TracksElemsOp,ElInfo el))      => TracksElemsQM    ms    ps m     el a          where elemsQM          = RUNOP() elemsM_ viewContainerM (const . return)



