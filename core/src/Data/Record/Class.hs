{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}


module Data.Record.Class where

import Prologue hiding (mask, simple, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, minBound, maxBound, (#), assert, Index, s)
import Prologue.Unsafe (fromJustNote)

import Type.Container
import Data.Result
import Type.Map

import Control.Monad.State hiding (when, withState)

import Data.Maybe (catMaybes)
import Data.Base
import Type.Bool
import Type.List ()

import qualified Control.Monad.State.Dependent as State
import Type.Cache.TH ()
import Type.Set ()
import Type.Zip ()
import Type.Maybe ()
import Type.Show ()
import Type.Sequence ()
import Type.Monoid ()
import Data.Cover
import Data.Shell ()


-----------------------------------------------------------------

withState f = do
    s <- get
    put (f s)
{-# INLINE withState #-}





-----------------
-- === NOP === --
-----------------

data NOP = NOP deriving (Show)




--------------------
-- === Record === --
--------------------

class IsRecord  a where asRecord :: Iso'  a (RecordOf a)
class HasRecord a where record   :: Lens' a (RecordOf a)
                        default record :: IsRecord a => Lens' a (RecordOf a)
                        record = asRecord

type family Props p a :: [*]
type family GatherProps p lst :: [*] where
    GatherProps p '[]       = '[]
    GatherProps p (l ': ls) = (l ## p) <> GatherProps p ls

type a ##  p = Props p a
type a ##. p = (RecordOf a) ## p

type Variants a = Props Variant a
type Groups   a = Props Group   a

type family Layout_Variants t a :: [*] -- TODO[WD]: Refactor and allow Layout to lookup for different layout aspects

type family RecordOf  a :: *
type family Layout    a :: [*]
type family EncodeMap a :: Map * [Nat]
type family DecodeMap a :: Map * Nat

type family Encode rec t :: [Nat] -- new interface
type family Decode rec t ::  Nat  -- new interface

mkRecord :: IsRecord a => RecordOf a -> a
mkRecord r = view (from asRecord) r
{-# INLINE mkRecord #-}

-- | `t` is the type of encoder, like Variant or Group
class Encoder t a m rec | t a rec -> m where encode :: Proxy t -> a -> m rec

-- TODO[WD]: Maybe unsafe extract / insert should be some kind of lens?
class UnsafeExtract t rec m a | t rec a -> m where unsafeExtract :: Proxy t -> rec -> m a
class UnsafeInsert  t rec m a | t rec a -> m where unsafeInsert  :: Proxy t -> a -> rec -> m rec
class CheckMatch    t a rec where checkMatch :: Proxy t -> Proxy (a :: *) -> rec -> Bool


-- === Instances === --

type instance RecordOf (Cover c a) = RecordOf a
instance HasRecord a => HasRecord (Cover c a) where record = covered ∘ record ; {-# INLINE record #-}

------------------------
-- === Properties === --
------------------------

data Value    = Value   deriving (Show)
data Variant  = Variant deriving (Show)
data Group    = Group   deriving (Show)


type family ValueOf a
class HasValue a where value :: Lens' a (ValueOf a)


type family ElemType  a rec where ElemType  a rec = ElemType' a (RecordOf rec)
type family ElemType' a rec where ElemType' a rec = If (a `In` Variants rec) ('Just Variant)
                                                  ( If (a `In` Groups   rec) ('Just Group) 'Nothing)


---- === Resolvers === --

type family ResolveGroups   a rec where ResolveGroups   a rec = ResolveAmong a (Groups   (RecordOf rec))
type family ResolveVariants a rec where ResolveVariants a rec = ResolveAmong a (Variants (RecordOf rec))
type family ResolveElems    a rec where ResolveElems    a rec = ResolveAmong a (Groups (RecordOf rec) <> Variants (RecordOf rec))

type family ResolveAmong  a els where ResolveAmong  a els = LookupIdx (Index (Base a) (Bases els)) els
type family LookupIdx (idx :: Maybe Nat) (els :: [*]) where
    LookupIdx ('Just idx) els = 'Just (Index2 idx els)
    LookupIdx 'Nothing    els = 'Nothing

type family ResolveConstraint t a :: Constraint where
    ResolveConstraint 'Nothing  a = ()
    ResolveConstraint ('Just t) a = (t ~ a)

type Resolved a rec = ResolveConstraint (ResolveElems a rec) a



--------------------
-- === Errors === --
--------------------

data Invalid t a
data InvalidVariant v
data InvalidGroup   g
data InvalidSubgroupOf subgroup rec
data InvalidElement e
data InvalidPattern p

type Invalid' t a = Error (Invalid t a)
type InvalidVariant' v = Error (InvalidVariant v)
type InvalidGroup'   g = Error (InvalidGroup   g)
type InvalidSubgroupOf' subgroup rec = Error (InvalidSubgroupOf subgroup rec)
type InvalidElement' e = Error (InvalidElement e)
type InvalidPattern' p = Error (InvalidPattern p)



--------------------------
-- === Construction === --
--------------------------

-- === Unchecked and checked constructors === --
-- | Facilitates construction of records from given variants and groups if they match declared variant options

class UncheckedElemCons    t v m rec |    t v rec -> m where uncheckedElemCons ::                       Proxy t -> v -> m rec
class CheckedElemCons      t v m rec |    t v rec -> m where checkedElemCons   ::                       Proxy t -> v -> m rec
class CheckedElemCons'  ok t v m rec | ok t v rec -> m where checkedElemCons'  :: Proxy (ok :: Bool) -> Proxy t -> v -> m rec

instance ( IsRecord r, Functor m, Encoder t a m (RecordOf r)
         )      => UncheckedElemCons t a m r where uncheckedElemCons t = mkRecord <∘> encode t ; {-# INLINE uncheckedElemCons #-}
instance m ~ IM => UncheckedElemCons t I m r where uncheckedElemCons   = impossible            ; {-# INLINE uncheckedElemCons #-}
instance m ~ IM => UncheckedElemCons t g m I where uncheckedElemCons   = impossible            ; {-# INLINE uncheckedElemCons #-}

instance ( CheckedElemCons' ok t v m r
         , ok ~ (v `In` (r ##. t))) => CheckedElemCons         t v m  r where checkedElemCons      = checkedElemCons' (p :: P ok) ; {-# INLINE checkedElemCons  #-}
instance                               CheckedElemCons         t I IM r where checkedElemCons      = impossible                   ; {-# INLINE checkedElemCons  #-}
instance                               CheckedElemCons         t v IM I where checkedElemCons      = impossible                   ; {-# INLINE checkedElemCons  #-}
instance UncheckedElemCons t v m r  => CheckedElemCons' 'True  t v m  r where checkedElemCons' _   = uncheckedElemCons            ; {-# INLINE checkedElemCons' #-}
instance m ~ Invalid' t v           => CheckedElemCons' 'False t v m  r where checkedElemCons' _ _ = const Error                  ; {-# INLINE checkedElemCons' #-}

-- Specific constructors

type UncheckedVariantCons = UncheckedElemCons Variant
type CheckedVariantCons   = CheckedElemCons   Variant
type UncheckedGroupCons   = UncheckedElemCons Group
type CheckedGroupCons     = CheckedElemCons   Group

uncheckedVariantCons :: UncheckedVariantCons v m rec => v -> m rec
uncheckedGroupCons   :: UncheckedGroupCons   g m rec => g -> m rec
uncheckedVariantCons = uncheckedElemCons (p :: P Variant) ; {-# INLINE uncheckedVariantCons #-}
uncheckedGroupCons   = uncheckedElemCons (p :: P Group)   ; {-# INLINE uncheckedGroupCons   #-}

checkedVariantCons :: CheckedVariantCons v m rec => v -> m rec
checkedGroupCons   :: CheckedGroupCons   g m rec => g -> m rec
checkedVariantCons = checkedElemCons (p :: P Variant) ; {-# INLINE checkedVariantCons #-}
checkedGroupCons   = checkedElemCons (p :: P Group)   ; {-# INLINE checkedGroupCons   #-}


-- === Automatic constructors === --
-- | Automatically chooses between Variant- and Group- Cons if the element matches one of variant or group declaration

class AutoCons      e m rec |     e rec -> m where autoCons  ::                           e -> m rec
class AutoCons' tgt e m rec | tgt e rec -> m where autoCons' :: Proxy (tgt :: Maybe *) -> e -> m rec

instance ( tgt ~ ElemType e r
         , AutoCons' tgt e m r
         ) => AutoCons e m  r where autoCons = autoCons' (p :: P tgt) ; {-# INLINE autoCons #-}
instance      AutoCons I IM r where autoCons = impossible             ; {-# INLINE autoCons #-}
instance      AutoCons e IM I where autoCons = impossible             ; {-# INLINE autoCons #-}

instance UncheckedVariantCons e m r => AutoCons' ('Just Variant) e m r where autoCons' _   = uncheckedVariantCons ; {-# INLINE autoCons' #-}
instance UncheckedGroupCons   e m r => AutoCons' ('Just Group)   e m r where autoCons' _   = uncheckedGroupCons   ; {-# INLINE autoCons' #-}
instance m ~ InvalidElement' e      => AutoCons' 'Nothing        e m r where autoCons' _ _ = Error                ; {-# INLINE autoCons' #-}


-- === Resolved automatic constructors === --

class ResolvedAutoCons a m rec | a rec -> m where resolvedCons :: a -> m rec
instance {-# OVERLAPPABLE #-} ( ResolveConstraint (ResolveElems a rec) a
                              , AutoCons a m rec ) => ResolvedAutoCons a m rec where resolvedCons = autoCons   ; {-# INLINE resolvedCons #-}
instance {-# OVERLAPPABLE #-} m ~ IM               => ResolvedAutoCons I m rec where resolvedCons = impossible ; {-# INLINE resolvedCons #-}
instance {-# OVERLAPPABLE #-} m ~ IM               => ResolvedAutoCons a m I   where resolvedCons = impossible ; {-# INLINE resolvedCons #-}

evalResolvedAutoCons :: ResolvedAutoCons a Ok rec => a -> rec
evalResolvedAutoCons = fromOk ∘ resolvedCons ; {-# INLINE evalResolvedAutoCons #-}

type TryResolvedAutoCons a m rec = (ResolvedAutoCons a m rec, MaybeResult m)
tryResolvedAutoCons :: TryResolvedAutoCons a m rec => a -> Maybe rec
tryResolvedAutoCons = maybeResult ∘ resolvedCons ; {-# INLINE tryResolvedAutoCons #-}


-- === User friendly constructors === --

class Cons a rec where cons :: a -> rec
instance ResolvedAutoCons a Ok rec => Cons a rec where cons = evalResolvedAutoCons ; {-# INLINE cons #-}
instance                              Cons I rec where cons = impossible           ; {-# INLINE cons #-}
instance                              Cons a I   where cons = impossible           ; {-# INLINE cons #-}

class TryCons a rec where tryCons :: a -> Maybe rec
instance TryResolvedAutoCons a m rec => TryCons a rec where tryCons = tryResolvedAutoCons ; {-# INLINE tryCons #-}
instance                                TryCons I rec where tryCons = impossible          ; {-# INLINE tryCons #-}
instance                                TryCons a I   where tryCons = impossible          ; {-# INLINE tryCons #-}

class SmartCons a rec where smartCons :: a -> rec
instance {-# OVERLAPPABLE #-} TryCons a rec => SmartCons a (Maybe rec) where smartCons = tryCons    ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-} Cons    a rec => SmartCons a rec         where smartCons = cons       ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-}                  SmartCons a (Maybe I)   where smartCons = impossible ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-}                  SmartCons I rec         where smartCons = impossible ; {-# INLINE smartCons #-}





------------------------------------------------------------------------
-- === Mapping over matched record element (Variant, Group, etc.) === --
------------------------------------------------------------------------

-- === Mapping data types === --

data ANY = ANY deriving (Show)

type instance Base ANY = Proxy ANY


-- === ElemMap === --

class ElemMap       t v m rec |    t v rec -> m where elemMap  :: forall a.                       Proxy t -> (v -> a) -> rec -> m (Maybe a)
class ElemMap'   ok t v m rec | ok t v rec -> m where elemMap' :: forall a. Proxy (ok :: Bool) -> Proxy t -> (v -> a) -> rec -> m (Maybe a)
type  ElemMapped    t v   rec = ElemMap t v Ok rec

type  UncheckedElemMap            = ElemMap' 'True
type  UncheckedElemMapped t v rec = ElemMap' 'True t v Ok rec

instance {-# OVERLAPPABLE #-} ( ok ~ (v `In` (r ##. t)) , ElemMap' ok t v m r )
                                     => ElemMap         t v   m r where elemMap          = elemMap' (p :: P ok) ; {-# INLINE elemMap  #-}
instance {-# OVERLAPPABLE #-} m ~ Ok => ElemMap         t ANY m r where elemMap  _ f _   = Ok $ Just $ f ANY    ; {-# INLINE elemMap  #-}
instance {-# OVERLAPPABLE #-} m ~ IM => ElemMap         t v   m I where elemMap  _ _ _   = impossible           ; {-# INLINE elemMap  #-}
instance m ~ Invalid' t v            => ElemMap' 'False t v   m r where elemMap' _ _ _ _ = Error                ; {-# INLINE elemMap' #-}
instance ( UnsafeExtract t rec m a
         , HasRecord r
         , rec ~ RecordOf r
         , Monad m
         , CheckMatch t a rec
         ) => ElemMap' 'True t a m r where
    elemMap' _ t f r = if match then (Just ∘ (f $) <$> val) else return Nothing where
        rec      = r ^. record
        match    = checkMatch t (p :: P a) rec
        val      = unsafeExtract t rec :: m a
    {-# INLINE elemMap' #-}

elemMapped :: ElemMapped t v rec => Proxy t -> (v -> a) -> rec -> Maybe a
elemMapped = fromOk ∘∘∘ elemMap ; {-# INLINE elemMapped #-}

uncheckedElemMap :: UncheckedElemMap t v m rec => Proxy t -> (v -> a) -> rec -> m (Maybe a)
uncheckedElemMap = elemMap' (Proxy :: Proxy 'True) ; {-# INLINE uncheckedElemMap #-}

uncheckedElemMapped :: UncheckedElemMapped t v rec => Proxy t -> (v -> a) -> rec -> Maybe a
uncheckedElemMapped = fromOk ∘∘∘ uncheckedElemMap ; {-# INLINE uncheckedElemMapped #-}

-- === VariantMap === --

type          VariantMap    v m rec =          ElemMap    Variant v m rec
type          VariantMapped v   rec =          ElemMapped Variant v   rec
type UncheckedVariantMap    v m rec = UncheckedElemMap    Variant v m rec
type UncheckedVariantMapped v   rec = UncheckedElemMapped Variant v   rec

variantMap             ::          VariantMap    v m rec => (v -> a) -> rec -> m (Maybe a)
variantMapped          ::          VariantMapped v   rec => (v -> a) -> rec ->    Maybe a
uncheckedVariantMap    :: UncheckedVariantMap    v m rec => (v -> a) -> rec -> m (Maybe a)
uncheckedVariantMapped :: UncheckedVariantMapped v   rec => (v -> a) -> rec ->    Maybe a

variantMap             = elemMap             (p :: P Variant) ; {-# INLINE variantMap             #-}
variantMapped          = elemMapped          (p :: P Variant) ; {-# INLINE variantMapped          #-}
uncheckedVariantMap    = uncheckedElemMap    (p :: P Variant) ; {-# INLINE uncheckedVariantMap    #-}
uncheckedVariantMapped = uncheckedElemMapped (p :: P Variant) ; {-# INLINE uncheckedVariantMapped #-}

-- === GroupMap === --

type GroupMap    v m rec = ElemMap    Group v m rec
type GroupMapped v   rec = ElemMapped Group v   rec

groupMap    :: GroupMap    g m rec => (g -> a) -> rec -> m (Maybe a)
groupMapped :: GroupMapped g   rec => (g -> a) -> rec ->    Maybe a
groupMap    = elemMap    (p :: P Group) ; {-# INLINE groupMap    #-}
groupMapped = elemMapped (p :: P Group) ; {-# INLINE groupMapped #-}




------------------------------------------------------------------------
-- === Mapping over current record element (Variant, Group, etc.) === --
------------------------------------------------------------------------


-- FIXME[WD]: poprawic ponizsze funkcje poniewaz sa bardzo zawezone - np WithElement_ dziala tylko na Variantach!

--class MyVariantMap where myVariantMap :: Proxy ctx -> (forall )

class WithElement' ctx rec a where withElement' :: Proxy ctx -> (forall v. ctx v a => v -> a) -> rec -> a
instance (MapTryingElemList els ctx rec a, els ~ Layout_Variants Variant (RecordOf rec)) => WithElement' ctx rec a where withElement' = mapTryingElemList (p :: P els)

class MapTryingElemList els ctx rec a where mapTryingElemList :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v a => v -> a) -> rec -> a

instance ( HasRecord r
         , rec ~ RecordOf r
         , CheckMatch Variant el rec
         , UnsafeExtract Variant rec Ok el
         , ctx el a
         , MapTryingElemList els ctx r a
         ) => MapTryingElemList (el ': els) ctx r a where
    mapTryingElemList _ ctx f r = if match then f el else mapTryingElemList (p :: P els) ctx f r where
        rec   = r ^. record
        t     = p :: P Variant
        match = checkMatch t (p :: P el) rec
        Ok el = unsafeExtract t rec :: Ok el


instance MapTryingElemList '[] ctx rec a --where mapTryingElemList = error "Data/Record special error"




class WithElement_ ctx rec where withElement_ :: Proxy ctx -> (forall v. ctx v => v -> a) -> rec -> a
instance (MapTryingElemList_ els ctx rec, els ~ (RecordOf rec ##. Variant)) => WithElement_ ctx rec where withElement_ = mapTryingElemList_ (p :: P els)

class MapTryingElemList_ els ctx rec where mapTryingElemList_ :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v => v -> a) -> rec -> a

instance ( HasRecord r
         , rec ~ RecordOf r
         , CheckMatch Variant el rec
         , UnsafeExtract Variant rec Ok el
         , ctx el
         , MapTryingElemList_ els ctx r
         ) => MapTryingElemList_ (el ': els) ctx r where
    mapTryingElemList_ _ ctx f r = if match then f el else mapTryingElemList_ (p :: P els) ctx f r where
        rec   = r ^. record
        t     = p :: P Variant
        match = checkMatch t (p :: P el) rec
        Ok el = unsafeExtract t rec :: Ok el


instance MapTryingElemList_ '[] ctx rec --where mapTryingElemList_ = error "Data/Record special error"

--class NFunctor n m a a' | n m a -> a' where fmapN :: (n -> m) -> a -> a'


class OverElement ctx rec where overElement :: Proxy ctx -> (forall v. ctx v => v -> v) -> rec -> rec
instance (MapOverElemList els ctx rec, els ~ (RecordOf rec ##. Variant)) => OverElement ctx rec where overElement = mapOverElemList (p :: P els)

class MapOverElemList els ctx rec where mapOverElemList :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v => v -> v) -> rec -> rec


instance MapOverElemList '[] ctx rec --where mapOverElemList = error "Data/Record special error"


instance ( HasRecord r
         , rec ~ RecordOf r
         , CheckMatch Variant el (RecordOf r)
         , UnsafeExtract Variant rec Ok el
         , UnsafeInsert  Variant rec Ok el
         , ctx el
         , MapOverElemList els ctx r
         ) => MapOverElemList (el ': els) ctx r where
    mapOverElemList _ ctx f r = if match then out else mapOverElemList (p :: P els) ctx f r where
        rec    = r ^. record
        t      = p :: P Variant
        match  = checkMatch t (p :: P el) rec
        Ok el  = unsafeExtract t rec :: Ok el
        Ok out = flip (set record) r <$> (unsafeInsert  t (f el) rec :: Ok rec) :: Ok r


--class UnsafeInsert  t rec m a | t rec a -> m where unsafeInsert  :: Proxy t -> a -> rec -> m rec


------------------------------
-- === Pattern matching === --
------------------------------

-- === Data declarations === --

data PM = PM deriving (Show)

newtype MatchState rec s a = MatchState (State.State PM [rec -> Maybe s] a) deriving (Functor, Applicative, Monad)
type    MatchSet   rec s   = MatchState rec s ()


-- === Basic matching === --

variantMatch :: VariantMapped a rec => (a -> out) -> MatchSet rec out
groupMatch   :: GroupMapped   a rec => (a -> out) -> MatchSet rec out
variantMatch f = MatchState $ withMatchSet (variantMapped f:) ; {-# INLINE variantMatch #-}
groupMatch   f = MatchState $ withMatchSet (groupMapped   f:) ; {-# INLINE groupMatch   #-}

uncheckedVariantMatch :: (rec ~ RecordOf r, UnsafeExtract Variant rec Ok v, HasRecord r, CheckMatch Variant v rec) => (v -> a) -> MatchState r a ()
uncheckedVariantMatch f = MatchState $ withMatchSet (uncheckedVariantMapped f:) ; {-# INLINE uncheckedVariantMatch #-}


-- Pattern match evaluation

withMatchSet :: State.MonadState PM s m => (s -> s) -> m ()
withMatchSet = State.withState PM
{-# INLINE withMatchSet #-}

matchedOptions :: rec -> MatchSet rec s -> [s]
matchedOptions t (MatchState s) = catMaybes ∘ reverse $ ($ t) <$> State.exec PM s [] ; {-# INLINE matchedOptions #-}

runMatches :: rec -> MatchSet rec s -> Maybe s
runMatches = tryHead ∘∘ matchedOptions ; {-# INLINE runMatches #-}

-- TODO [WD]: Add TH case' interface
__case__ lib file loc = fromJustNote err ∘∘ runMatches where
    err = lib <> ": " <> file <> ":" <> show loc <> ": Non-exhaustive patterns in case"
{-# INLINE __case__ #-}

-- FIXME [WD]: Remove caseTest
caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}


-- === Auto matches === --

class AutoMatch      a m rec |     a rec -> m where autoMatch  ::                           forall out. (a -> out) -> m (MatchSet rec out)
class AutoMatch' tgt a m rec | tgt a rec -> m where autoMatch' :: Proxy (tgt :: Maybe *) -> forall out. (a -> out) -> m (MatchSet rec out)

instance {-# OVERLAPPABLE #-} (tgt ~ ElemType a rec, AutoMatch' tgt a m rec) => AutoMatch                  a   m rec where autoMatch    = autoMatch' (p :: P tgt) ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok)                                       => AutoMatch                  ANY m rec where autoMatch    = Ok ∘ variantMatch       ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ IM)                                       => AutoMatch                  a   m I   where autoMatch    = impossible              ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok, VariantMapped a rec)                  => AutoMatch' ('Just Variant) a   m rec where autoMatch' _ = Ok ∘ variantMatch       ; {-# INLINE autoMatch' #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok, GroupMapped   a rec)                  => AutoMatch' ('Just Group)   a   m rec where autoMatch' _ = Ok ∘ groupMatch         ; {-# INLINE autoMatch' #-}
instance {-# OVERLAPPABLE #-} (m ~ InvalidPattern' a)                        => AutoMatch'  'Nothing       a   m rec where autoMatch' _ = const Error             ; {-# INLINE autoMatch' #-}


-- === General matching interface === --

class                                                                 Match a rec where of' :: forall out. (a -> out) -> MatchSet rec out
instance {-# OVERLAPPABLE #-} (Resolved a rec, AutoMatch a Ok rec) => Match a rec where of' = fromOk ∘ autoMatch ; {-# INLINE of' #-}
instance {-# OVERLAPPABLE #-}                                         Match I rec where of' = impossible         ; {-# INLINE of' #-}
instance {-# OVERLAPPABLE #-}                                         Match a I   where of' = impossible         ; {-# INLINE of' #-}

type family Matches rec lst :: Constraint where
    Matches rec '[]       = ()
    Matches rec (l ': ls) = (Match l rec, Matches rec ls)

-- === Static-AST matching === --

static :: (Match rec' rec) => (rec' -> out) -> MatchSet rec out
static = of'



-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------

-- TODO[WD] refactor vvvvvvvvvvvvvvv



-- FIXME[WD]: Remove the following instances. They are very dangerous and are a anti-pattern in HS.
instance UncheckedGroupCons rec m a                                                              => UnsafeExtract Group   rec      m  a where unsafeExtract _  = uncheckedGroupCons          ; {-# INLINE unsafeExtract #-}
-- instance {-# OVERLAPPABLE #-} (UnsafeExtract Variant (Unwrapped rec) m a, Wrapped rec)           => UnsafeExtract Variant rec      m  a where unsafeExtract t  = unsafeExtract t ∘ unwrap'   ; {-# INLINE unsafeExtract #-}
-- instance {-# OVERLAPPABLE #-} (UnsafeInsert Variant (Unwrapped rec) m a, Wrapped rec, Functor m) => UnsafeInsert  Variant rec      m  a where unsafeInsert t a = wrapped' $ unsafeInsert t a ; {-# INLINE unsafeInsert  #-}
