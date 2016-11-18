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
{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Record.Class where

import Prologue hiding (mask, simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)
import Prologue.Unsafe (fromJustNote, error)

import Type.Container hiding (FromJust)

-- import Old.Luna.Runtime.Dynamics (Static, Dynamic)
import qualified Old.Luna.Runtime.Dynamics as Runtime
--import Data.Bits.Mask         (Mask)
import GHC.Prim            (Any, unsafeCoerce#)
import Data.Int            (Int64)

import Unsafe.Coerce (unsafeCoerce)
import Data.Result

import Data.Bits (Bits, FiniteBits, setBit, testBit, zeroBits, finiteBitSize)

import Type.Map

import Data.Foldable (foldl', foldr')
import Control.Monad.State hiding (when, withState)

import Data.Maybe (isNothing, catMaybes)
import Data.Base
import Type.Bool
import Type.List hiding (FromJust)
import System.Environment (getArgs)
import Data.List.Split (chunksOf)

import qualified Control.Monad.State.Dependent as State
import Type.Cache.TH
import Type.Set
import Type.Zip
import Type.Maybe
import Type.Show
import Type.Sequence          (Enumerate)
import Type.Monoid
import Data.Cover
import Data.Shell


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

type family Encode rrr t :: [Nat] -- new interface
type family Decode rrr t ::  Nat  -- new interface

type family Encode2 t a :: Maybe Nat -- new interface 2 -- e.g Encode Atom Blank

type family MapEncode t fmts where
    MapEncode t '[]       = '[]
    MapEncode t (f ': fs) = FromJust (Encode2 t f) ': MapEncode t fs

encode3 :: forall t a. KnownNat (FromJust (Encode2 t a)) => a -> Int
encode3 (a :: a) = fromIntegral $ natVal (p :: P (FromJust (Encode2 t a)))
{-# INLINE encode3 #-}

encodeType :: forall t a. KnownNat (FromJust (Encode2 t a)) => Int
encodeType = fromIntegral $ natVal (p :: P (FromJust (Encode2 t a)))
{-# INLINE encodeType #-}

mkRecord :: IsRecord a => RecordOf a -> a
mkRecord r = view (from asRecord) r
{-# INLINE mkRecord #-}

-- | `t` is the type of encoder, like Variant or Group
class Encoder t a m rrr | t a rrr -> m where encode :: Proxy t -> a -> m rrr

-- TODO[WD]: Maybe unsafe extract / insert should be some kind of lens?
class UnsafeExtract t rrr m a | t rrr a -> m where unsafeExtract :: Proxy t -> rrr -> m a
class UnsafeInsert  t rrr m a | t rrr a -> m where unsafeInsert  :: Proxy t -> a -> rrr -> m rrr
class CheckMatch    t a rrr where checkMatch :: Proxy t -> Proxy (a :: *) -> rrr -> Bool


-- === Instances === --

type instance RecordOf (Cover c a) = RecordOf a
-- instance HasRecord a => HasRecord (Cover c a) where record = covered ∘ record ; {-# INLINE record #-}

------------------------
-- === Properties === --
------------------------

data Value    = Value   deriving (Show)
data Variant  = Variant deriving (Show)
data Group    = Group   deriving (Show)


type family ValueOf a
class HasValue a where value :: Lens' a (ValueOf a)


type family ElemType  a rrr where ElemType  a rrr = ElemType' a (RecordOf rrr)
type family ElemType' a rrr where ElemType' a rrr = If (a `In` Variants rrr) ('Just Variant)
                                                  ( If (a `In` Groups   rrr) ('Just Group) 'Nothing)


---- === Resolvers === --

type family ResolveGroups   a rrr where ResolveGroups   a rrr = ResolveAmong a (Groups   (RecordOf rrr))
type family ResolveVariants a rrr where ResolveVariants a rrr = ResolveAmong a (Variants (RecordOf rrr))
type family ResolveElems    a rrr where ResolveElems    a rrr = ResolveAmong a (Groups (RecordOf rrr) <> Variants (RecordOf rrr))

type family ResolveAmong  a els where ResolveAmong  a els = LookupIdx (Index (Base a) (Bases els)) els
type family LookupIdx (idx :: Maybe Nat) (els :: [*]) where
    LookupIdx ('Just idx) els = 'Just (Index2 idx els)
    LookupIdx 'Nothing    els = 'Nothing

type family ResolveConstraint t a :: Constraint where
    ResolveConstraint 'Nothing  a = ()
    ResolveConstraint ('Just t) a = (t ~ a)

type Resolved a rrr = ResolveConstraint (ResolveElems a rrr) a



--------------------
-- === Errors === --
--------------------

data Invalid t a
data InvalidVariant v
data InvalidGroup   g
data InvalidSubgroupOf subgroup rrr
data InvalidElement e
data InvalidPattern p

type Invalid' t a = Error (Invalid t a)
type InvalidVariant' v = Error (InvalidVariant v)
type InvalidGroup'   g = Error (InvalidGroup   g)
type InvalidSubgroupOf' subgroup rrr = Error (InvalidSubgroupOf subgroup rrr)
type InvalidElement' e = Error (InvalidElement e)
type InvalidPattern' p = Error (InvalidPattern p)



--------------------------
-- === Construction === --
--------------------------

-- === Unchecked and checked constructors === --
-- | Facilitates construction of records from given variants and groups if they match declared variant options

class UncheckedElemCons    t v m rrr |    t v rrr -> m where uncheckedElemCons ::                       Proxy t -> v -> m rrr
class CheckedElemCons      t v m rrr |    t v rrr -> m where checkedElemCons   ::                       Proxy t -> v -> m rrr
class CheckedElemCons'  ok t v m rrr | ok t v rrr -> m where checkedElemCons'  :: Proxy (ok :: Bool) -> Proxy t -> v -> m rrr

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

uncheckedVariantCons :: UncheckedVariantCons v m rrr => v -> m rrr
uncheckedGroupCons   :: UncheckedGroupCons   g m rrr => g -> m rrr
uncheckedVariantCons = uncheckedElemCons (p :: P Variant) ; {-# INLINE uncheckedVariantCons #-}
uncheckedGroupCons   = uncheckedElemCons (p :: P Group)   ; {-# INLINE uncheckedGroupCons   #-}

checkedVariantCons :: CheckedVariantCons v m rrr => v -> m rrr
checkedGroupCons   :: CheckedGroupCons   g m rrr => g -> m rrr
checkedVariantCons = checkedElemCons (p :: P Variant) ; {-# INLINE checkedVariantCons #-}
checkedGroupCons   = checkedElemCons (p :: P Group)   ; {-# INLINE checkedGroupCons   #-}


-- === Automatic constructors === --
-- | Automatically chooses between Variant- and Group- Cons if the element matches one of variant or group declaration

class AutoCons      e m rrr |     e rrr -> m where autoCons  ::                           e -> m rrr
class AutoCons' tgt e m rrr | tgt e rrr -> m where autoCons' :: Proxy (tgt :: Maybe *) -> e -> m rrr

instance ( tgt ~ ElemType e r
         , AutoCons' tgt e m r
         ) => AutoCons e m  r where autoCons = autoCons' (p :: P tgt) ; {-# INLINE autoCons #-}
instance      AutoCons I IM r where autoCons = impossible             ; {-# INLINE autoCons #-}
instance      AutoCons e IM I where autoCons = impossible             ; {-# INLINE autoCons #-}

instance UncheckedVariantCons e m r => AutoCons' ('Just Variant) e m r where autoCons' _   = uncheckedVariantCons ; {-# INLINE autoCons' #-}
instance UncheckedGroupCons   e m r => AutoCons' ('Just Group)   e m r where autoCons' _   = uncheckedGroupCons   ; {-# INLINE autoCons' #-}
instance m ~ InvalidElement' e      => AutoCons' 'Nothing        e m r where autoCons' _ _ = Error                ; {-# INLINE autoCons' #-}


-- === Resolved automatic constructors === --

class ResolvedAutoCons a m rrr | a rrr -> m where resolvedCons :: a -> m rrr
instance {-# OVERLAPPABLE #-} ( ResolveConstraint (ResolveElems a rrr) a
                              , AutoCons a m rrr ) => ResolvedAutoCons a m rrr where resolvedCons = autoCons   ; {-# INLINE resolvedCons #-}
instance {-# OVERLAPPABLE #-} m ~ IM               => ResolvedAutoCons I m rrr where resolvedCons = impossible ; {-# INLINE resolvedCons #-}
instance {-# OVERLAPPABLE #-} m ~ IM               => ResolvedAutoCons a m I   where resolvedCons = impossible ; {-# INLINE resolvedCons #-}

evalResolvedAutoCons :: ResolvedAutoCons a Ok rrr => a -> rrr
evalResolvedAutoCons = fromOk ∘ resolvedCons ; {-# INLINE evalResolvedAutoCons #-}

type TryResolvedAutoCons a m rrr = (ResolvedAutoCons a m rrr, MaybeResult m)
tryResolvedAutoCons :: TryResolvedAutoCons a m rrr => a -> Maybe rrr
tryResolvedAutoCons = maybeResult ∘ resolvedCons ; {-# INLINE tryResolvedAutoCons #-}


-- === User friendly constructors === --

class Cons a rrr where cons :: a -> rrr
instance ResolvedAutoCons a Ok rrr => Cons a rrr where cons = evalResolvedAutoCons ; {-# INLINE cons #-}
instance                              Cons I rrr where cons = impossible           ; {-# INLINE cons #-}
instance                              Cons a I   where cons = impossible           ; {-# INLINE cons #-}

class TryCons a rrr where tryCons :: a -> Maybe rrr
instance TryResolvedAutoCons a m rrr => TryCons a rrr where tryCons = tryResolvedAutoCons ; {-# INLINE tryCons #-}
instance                                TryCons I rrr where tryCons = impossible          ; {-# INLINE tryCons #-}
instance                                TryCons a I   where tryCons = impossible          ; {-# INLINE tryCons #-}

class SmartCons a rrr where smartCons :: a -> rrr
instance {-# OVERLAPPABLE #-} TryCons a rrr => SmartCons a (Maybe rrr) where smartCons = tryCons    ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-} Cons    a rrr => SmartCons a rrr         where smartCons = cons       ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-}                  SmartCons a (Maybe I)   where smartCons = impossible ; {-# INLINE smartCons #-}
instance {-# OVERLAPPABLE #-}                  SmartCons I rrr         where smartCons = impossible ; {-# INLINE smartCons #-}





------------------------------------------------------------------------
-- === Mapping over matched record element (Variant, Group, etc.) === --
------------------------------------------------------------------------

-- === Mapping data types === --

data ANY = ANY deriving (Show)

type instance Base ANY = Proxy ANY


-- === ElemMap === --

class ElemMap       t v m rrr |    t v rrr -> m where elemMap  :: forall a.                       Proxy t -> (v -> a) -> rrr -> m (Maybe a)
class ElemMap'   ok t v m rrr | ok t v rrr -> m where elemMap' :: forall a. Proxy (ok :: Bool) -> Proxy t -> (v -> a) -> rrr -> m (Maybe a)
type  ElemMapped    t v   rrr = ElemMap t v Ok rrr

type  UncheckedElemMap            = ElemMap' 'True
type  UncheckedElemMapped t v rrr = ElemMap' 'True t v Ok rrr

instance {-# OVERLAPPABLE #-} ( ok ~ (v `In` (r ##. t)) , ElemMap' ok t v m r )
                                     => ElemMap         t v   m r where elemMap          = elemMap' (p :: P ok) ; {-# INLINE elemMap  #-}
instance {-# OVERLAPPABLE #-} m ~ Ok => ElemMap         t ANY m r where elemMap  _ f _   = Ok $ Just $ f ANY    ; {-# INLINE elemMap  #-}
instance {-# OVERLAPPABLE #-} m ~ IM => ElemMap         t v   m I where elemMap  _ _ _   = impossible           ; {-# INLINE elemMap  #-}
instance m ~ Invalid' t v            => ElemMap' 'False t v   m r where elemMap' _ _ _ _ = Error                ; {-# INLINE elemMap' #-}
instance ( UnsafeExtract t rrr m a
         , HasRecord r
         , rrr ~ RecordOf r
         , Monad m
         , CheckMatch t a rrr
         ) => ElemMap' 'True t a m r where
    elemMap' _ t f r = if match then (Just ∘ (f $) <$> val) else return Nothing where
        rrr      = r ^. record
        match    = checkMatch t (p :: P a) rrr
        val      = unsafeExtract t rrr :: m a
    {-# INLINE elemMap' #-}

elemMapped :: ElemMapped t v rrr => Proxy t -> (v -> a) -> rrr -> Maybe a
elemMapped = fromOk ∘∘∘ elemMap ; {-# INLINE elemMapped #-}

uncheckedElemMap :: UncheckedElemMap t v m rrr => Proxy t -> (v -> a) -> rrr -> m (Maybe a)
uncheckedElemMap = elemMap' (Proxy :: Proxy 'True) ; {-# INLINE uncheckedElemMap #-}

uncheckedElemMapped :: UncheckedElemMapped t v rrr => Proxy t -> (v -> a) -> rrr -> Maybe a
uncheckedElemMapped = fromOk ∘∘∘ uncheckedElemMap ; {-# INLINE uncheckedElemMapped #-}

-- === VariantMap === --

type          VariantMap    v m rrr =          ElemMap    Variant v m rrr
type          VariantMapped v   rrr =          ElemMapped Variant v   rrr
type UncheckedVariantMap    v m rrr = UncheckedElemMap    Variant v m rrr
type UncheckedVariantMapped v   rrr = UncheckedElemMapped Variant v   rrr

variantMap             ::          VariantMap    v m rrr => (v -> a) -> rrr -> m (Maybe a)
variantMapped          ::          VariantMapped v   rrr => (v -> a) -> rrr ->    Maybe a
uncheckedVariantMap    :: UncheckedVariantMap    v m rrr => (v -> a) -> rrr -> m (Maybe a)
uncheckedVariantMapped :: UncheckedVariantMapped v   rrr => (v -> a) -> rrr ->    Maybe a

variantMap             = elemMap             (p :: P Variant) ; {-# INLINE variantMap             #-}
variantMapped          = elemMapped          (p :: P Variant) ; {-# INLINE variantMapped          #-}
uncheckedVariantMap    = uncheckedElemMap    (p :: P Variant) ; {-# INLINE uncheckedVariantMap    #-}
uncheckedVariantMapped = uncheckedElemMapped (p :: P Variant) ; {-# INLINE uncheckedVariantMapped #-}

-- === GroupMap === --

type GroupMap    v m rrr = ElemMap    Group v m rrr
type GroupMapped v   rrr = ElemMapped Group v   rrr

groupMap    :: GroupMap    g m rrr => (g -> a) -> rrr -> m (Maybe a)
groupMapped :: GroupMapped g   rrr => (g -> a) -> rrr ->    Maybe a
groupMap    = elemMap    (p :: P Group) ; {-# INLINE groupMap    #-}
groupMapped = elemMapped (p :: P Group) ; {-# INLINE groupMapped #-}




------------------------------------------------------------------------
-- === Mapping over current record element (Variant, Group, etc.) === --
------------------------------------------------------------------------


-- FIXME[WD]: poprawic ponizsze funkcje poniewaz sa bardzo zawezone - np WithElement_ dziala tylko na Variantach!

--class MyVariantMap where myVariantMap :: Proxy ctx -> (forall )

class WithElement' ctx rrr a where withElement' :: Proxy ctx -> (forall v. ctx v a => v -> a) -> rrr -> a
instance (MapTryingElemList els ctx rrr a, els ~ Layout_Variants Variant (RecordOf rrr)) => WithElement' ctx rrr a where withElement' = mapTryingElemList (p :: P els)

class MapTryingElemList els ctx rrr a where mapTryingElemList :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v a => v -> a) -> rrr -> a

instance ( HasRecord r
         , rrr ~ RecordOf r
         , CheckMatch Variant el rrr
         , UnsafeExtract Variant rrr Ok el
         , ctx el a
         , MapTryingElemList els ctx r a
         ) => MapTryingElemList (el ': els) ctx r a where
    mapTryingElemList _ ctx f r = if match then f el else mapTryingElemList (p :: P els) ctx f r where
        rrr   = r ^. record
        t     = p :: P Variant
        match = checkMatch t (p :: P el) rrr
        Ok el = unsafeExtract t rrr :: Ok el


instance MapTryingElemList '[] ctx rrr a --where mapTryingElemList = error "Data/Record special error"




class WithElement_ ctx rrr where withElement_ :: Proxy ctx -> (forall v. ctx v => v -> a) -> rrr -> a
instance (MapTryingElemList_ els ctx rrr, els ~ (RecordOf rrr ##. Variant)) => WithElement_ ctx rrr where withElement_ = mapTryingElemList_ (p :: P els)

class MapTryingElemList_ els ctx rrr where mapTryingElemList_ :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v => v -> a) -> rrr -> a

instance ( HasRecord r
         , rrr ~ RecordOf r
         , CheckMatch Variant el rrr
         , UnsafeExtract Variant rrr Ok el
         , ctx el
         , MapTryingElemList_ els ctx r
         ) => MapTryingElemList_ (el ': els) ctx r where
    mapTryingElemList_ _ ctx f r = if match then f el else mapTryingElemList_ (p :: P els) ctx f r where
        rrr   = r ^. record
        t     = p :: P Variant
        match = checkMatch t (p :: P el) rrr
        Ok el = unsafeExtract t rrr :: Ok el


instance MapTryingElemList_ '[] ctx rrr --where mapTryingElemList_ = error "Data/Record special error"

--class NFunctor n m a a' | n m a -> a' where fmapN :: (n -> m) -> a -> a'


class OverElement ctx rrr where overElement :: Proxy ctx -> (forall v. ctx v => v -> v) -> rrr -> rrr
instance (MapOverElemList els ctx rrr, els ~ (RecordOf rrr ##. Variant)) => OverElement ctx rrr where overElement = mapOverElemList (p :: P els)

class MapOverElemList els ctx rrr where mapOverElemList :: Proxy (els :: [*]) -> Proxy ctx -> (forall v. ctx v => v -> v) -> rrr -> rrr


instance MapOverElemList '[] ctx rrr --where mapOverElemList = error "Data/Record special error"


instance ( HasRecord r
         , rrr ~ RecordOf r
         , CheckMatch Variant el (RecordOf r)
         , UnsafeExtract Variant rrr Ok el
         , UnsafeInsert  Variant rrr Ok el
         , ctx el
         , MapOverElemList els ctx r
         ) => MapOverElemList (el ': els) ctx r where
    mapOverElemList _ ctx f r = if match then out else mapOverElemList (p :: P els) ctx f r where
        rrr    = r ^. record
        t      = p :: P Variant
        match  = checkMatch t (p :: P el) rrr
        Ok el  = unsafeExtract t rrr :: Ok el
        Ok out = flip (set record) r <$> (unsafeInsert  t (f el) rrr :: Ok rrr) :: Ok r


--class UnsafeInsert  t rrr m a | t rrr a -> m where unsafeInsert  :: Proxy t -> a -> rrr -> m rrr


------------------------------
-- === Pattern matching === --
------------------------------

-- === Data declarations === --

data PM = PM deriving (Show)

newtype MatchState rrr s a = MatchState (State.State PM [rrr -> Maybe s] a) deriving (Functor, Applicative, Monad)
type    MatchSet   rrr s   = MatchState rrr s ()


-- === Basic matching === --

variantMatch :: VariantMapped a rrr => (a -> out) -> MatchSet rrr out
groupMatch   :: GroupMapped   a rrr => (a -> out) -> MatchSet rrr out
variantMatch f = MatchState $ withMatchSet (variantMapped f:) ; {-# INLINE variantMatch #-}
groupMatch   f = MatchState $ withMatchSet (groupMapped   f:) ; {-# INLINE groupMatch   #-}

uncheckedVariantMatch :: (rrr ~ RecordOf r, UnsafeExtract Variant rrr Ok v, HasRecord r, CheckMatch Variant v rrr) => (v -> a) -> MatchState r a ()
uncheckedVariantMatch f = MatchState $ withMatchSet (uncheckedVariantMapped f:) ; {-# INLINE uncheckedVariantMatch #-}


-- Pattern match evaluation

withMatchSet :: State.MonadState PM s m => (s -> s) -> m ()
withMatchSet = State.withState PM
{-# INLINE withMatchSet #-}

matchedOptions :: rrr -> MatchSet rrr s -> [s]
matchedOptions t (MatchState s) = catMaybes ∘ reverse $ ($ t) <$> State.exec PM s [] ; {-# INLINE matchedOptions #-}

runMatches :: rrr -> MatchSet rrr s -> Maybe s
runMatches = tryHead ∘∘ matchedOptions ; {-# INLINE runMatches #-}

-- TODO [WD]: Add TH case' interface
__case__ lib file loc = fromJustNote err ∘∘ runMatches where
    err = lib <> ": " <> file <> ":" <> show loc <> ": Non-exhaustive patterns in case"
{-# INLINE __case__ #-}

-- FIXME [WD]: Remove caseTest
caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}


-- === Auto matches === --

class AutoMatch      a m rrr |     a rrr -> m where autoMatch  ::                           forall out. (a -> out) -> m (MatchSet rrr out)
class AutoMatch' tgt a m rrr | tgt a rrr -> m where autoMatch' :: Proxy (tgt :: Maybe *) -> forall out. (a -> out) -> m (MatchSet rrr out)

instance {-# OVERLAPPABLE #-} (tgt ~ ElemType a rrr, AutoMatch' tgt a m rrr) => AutoMatch                  a   m rrr where autoMatch    = autoMatch' (p :: P tgt) ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok)                                       => AutoMatch                  ANY m rrr where autoMatch    = Ok ∘ variantMatch       ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ IM)                                       => AutoMatch                  a   m I   where autoMatch    = impossible              ; {-# INLINE autoMatch  #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok, VariantMapped a rrr)                  => AutoMatch' ('Just Variant) a   m rrr where autoMatch' _ = Ok ∘ variantMatch       ; {-# INLINE autoMatch' #-}
instance {-# OVERLAPPABLE #-} (m ~ Ok, GroupMapped   a rrr)                  => AutoMatch' ('Just Group)   a   m rrr where autoMatch' _ = Ok ∘ groupMatch         ; {-# INLINE autoMatch' #-}
instance {-# OVERLAPPABLE #-} (m ~ InvalidPattern' a)                        => AutoMatch'  'Nothing       a   m rrr where autoMatch' _ = const Error             ; {-# INLINE autoMatch' #-}


-- === General matching interface === --

class                                                                 Match a rrr where of' :: forall out. (a -> out) -> MatchSet rrr out
instance {-# OVERLAPPABLE #-} (Resolved a rrr, AutoMatch a Ok rrr) => Match a rrr where of' = fromOk ∘ autoMatch ; {-# INLINE of' #-}
instance {-# OVERLAPPABLE #-}                                         Match I rrr where of' = impossible         ; {-# INLINE of' #-}
instance {-# OVERLAPPABLE #-}                                         Match a I   where of' = impossible         ; {-# INLINE of' #-}

type family Matches rrr lst :: Constraint where
    Matches rrr '[]       = ()
    Matches rrr (l ': ls) = (Match l rrr, Matches rrr ls)

-- === Static-AST matching === --

-- static :: (rrr' ~ To Static rrr, Match rrr' rrr) => (rrr' -> out) -> MatchSet rrr out
-- static = of'



-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------

-- TODO[WD] refactor vvvvvvvvvvvvvvv



-- FIXME[WD]: Remove the following instances. They are very dangerous and are a anti-pattern in HS.
instance UncheckedGroupCons rrr m a                                                              => UnsafeExtract Group   rrr      m  a where unsafeExtract _  = uncheckedGroupCons          ; {-# INLINE unsafeExtract #-}
-- instance {-# OVERLAPPABLE #-} (UnsafeExtract Variant (Unwrapped rrr) m a, Wrapped rrr)           => UnsafeExtract Variant rrr      m  a where unsafeExtract t  = unsafeExtract t ∘ unwrap'   ; {-# INLINE unsafeExtract #-}
-- instance {-# OVERLAPPABLE #-} (UnsafeInsert Variant (Unwrapped rrr) m a, Wrapped rrr, Functor m) => UnsafeInsert  Variant rrr      m  a where unsafeInsert t a = wrapped' $ unsafeInsert t a ; {-# INLINE unsafeInsert  #-}
