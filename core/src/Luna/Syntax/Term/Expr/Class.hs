{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}




module Luna.Syntax.Term.Expr.Class where


import           Prelude.Luna                 hiding (Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons, Value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty, FromJust)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import qualified Data.Reprx                   as Repr
import           Type.Bool
import           Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Symbol (Symbol)
import Luna.Syntax.Term.Expr.Atom

import Data.Shell               as Shell hiding (Access)
import Data.Record.Model.Masked as X (Data, Data2, TermRecord, VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)
import Luna.Syntax.Term.Expr (NameByDynamics)
import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import qualified Data.RTuple as List
import Type.Promotion    (KnownNats, natVals)
import Data.Bits         (setBit, zeroBits)

import Data.Container.Hetero (Elems)
import Data.RTuple (List, Empty, empty, Access, Accessible', accessProxy')
import Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), Data2(..), unsafeRestore, decodeNat)
import           Data.RTuple (TMap(..), empty) -- refactor empty to another library

import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)))
import Type.Error (Assert)
import Control.Monad.State
import Control.Lens.Property hiding (get)
import qualified Control.Lens.Property as Prop
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import Control.Monad.ST (ST, runST)
import Type.List (Index, Size)
import Type.Maybe (FromJust)
import Data.Phantom
import Unsafe.Coerce     (unsafeCoerce)
import Type.Relation (SemiSuper)

data {-kind-} Layout dyn form = Layout dyn form deriving (Show)

data Scope   = Scope   deriving (Show)
data Binding = Binding deriving (Show)
data Model   = Model   deriving (Show)

type family Bound layout (attrs :: [*]) :: * -- to musi byc tak zadeklarowane, poniewaz w ``(Binding (term # Layout) term)`, `term` moze byc zbyt rozpakowanego typu



data a := b

-- TO REFACTOR:
type instance View l ((k := v) ': ls) = If (k == l) v (ls ^. l)


data EE = EE -- TODO: refactor to Expr selector

type PossibleVariants = [Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type PossibleFormats  = [Literal, Value, Thunk, Phrase, Draft]


-------------------
-- === Terms === --
-------------------

-- === Definitions === --

newtype Term (attrs :: [*]) tags = Term (TMap tags (TermDatas attrs tags))

type family TermData  (attrs :: [*]) tag
type family TermDatas (attrs :: [*]) tags where
    TermDatas attrs '[] = '[]
    TermDatas attrs (t ': ts) = TermData attrs t ': TermDatas attrs ts


-- === Instances === --

-- Wrapper
makeWrapped ''Term

-- Show
instance Show (List (TermDatas attrs tags)) => Show (Term attrs tags) where
    showsPrec d (Term t) = showParen (d > app_prec) $
        showString "Term " . showsPrec (app_prec+1) (unwrap t)
        where app_prec = 10


-- Construction
instance tags ~ '[] => Empty (Term attrs tags) where
    empty = Term empty ; {-# INLINE empty #-}


-- Property access

type instance Get t (Term attrs tags) = Get t (Unwrapped (Term attrs tags))

instance Getter t (Unwrapped (Term attrs tags))
      => Getter t (Term attrs tags) where get = Prop.get @t . unwrap'

-- instance Accessible' ExprData (Term attrs tags)
--       => HasRecord2 (Term attrs tags) where record2 = List.access' ExprData


-- type instance Access p (Term attrs tags) = Access p (Unwrapped (Term attrs tags))
--
-- instance Accessible' p (Unwrapped (Term attrs tags))
--       => Accessible' p (Term attrs tags) where
--          accessProxy' p = wrapped . accessProxy' p ; {-# INLINE accessProxy' #-}



------------------
-- === Expr === --
------------------

-- === Definitions === --

type Expr2 binding attrs layers model scope = Term ( Binding := binding
                                                  ': Model   := model
                                                  ': Scope   := scope
                                                  ': attrs
                                                   ) (Symbol ': layers)

type Expr2' binding attrs layers model = Expr2 binding attrs layers model model


-- === ExprRecord === --

newtype ExprRecord2 bind model scope = ExprRecord2 Data4 deriving (Show)
type Data4 = Store2 '[ 'Slot Enum Atom, 'Slot Mask Format, 'Slot Raw Symbol ]

makeWrapped ''ExprRecord2


-- === Expr layer === --


type instance TermData attrs Symbol = ExprRecord2 (Bound (attrs ^. Binding) attrs) (attrs ^. Model) (attrs ^. Scope)


type instance TermData attrs Int      = Int




---------------------
-- === Records === --
---------------------


-- === Relations === --

-- | Variants is needed for operations that need to know all possible variant types
--   like maps that should invoke some action on current variant (e.g. `show`).
type family Variants3 rec :: [*]

type family InferVariant rec v :: Constraint

-- === Construction === --

class Monad m => Cons2 v m t where
    cons2 :: v -> m t


instance (Monad m, EncodeStore slots a m) => Cons2 a m (Store2 slots) where
    cons2 = encodeStore ; {-# INLINE cons2 #-}









-- instance (Monad m, List.Generate (Cons2 v) m (TermLayers attrs ls))
--       => Cons2 v m (Term attrs ls) where
--          cons2 v = (Term . Stack . TMap) <$> List.generate (Proxy :: Proxy (Cons2 v)) (cons2 v) ; {-# INLINE cons2 #-}

instance (Monad m, List.Generate (Cons2 v) m (TermDatas attrs ls))
      => Cons2 v m (Term attrs ls) where
         cons2 v = (Term . TMap) <$> List.generate (Proxy :: Proxy (Cons2 v)) (cons2 v) ; {-# INLINE cons2 #-}


-- instance (Monad m, Cons2 v m (LayerData l))
--       => Cons2 v m (Layer l) where
--          cons2 v = Shell.Layer <$> cons2 v ; {-# INLINE cons2 #-}

-- instance Monad m => Cons2 v m (Term attrs '[]) where
--     cons2 _ = return empty ; {-# INLINE cons2 #-}


type InvalidFormatAtom atom format = 'Text "Atom `" :<>: 'ShowType atom :<>: 'Text "` is not a valid for format `" :<>: 'ShowType format :<>: 'Text "`"

instance ( Monad m
         , Cons2 (Symbol.Data atom dyn bind) m Data4
         {-constraint solving-}
         , dyn  ~ dyn'
         , bind ~ bind'
         , Assert (atom `In` Atoms layout) (InvalidFormatAtom atom layout))
      => Cons2 (Symbol.Data atom dyn bind) m (ExprRecord2 bind' model (Layout dyn' layout)) where
    cons2 v = ExprRecord2 <$> cons2 v ; {-# INLINE cons2 #-}


instance Monad m => Cons2 v m Int where cons2 _ = return 5



-- === Pattern Matching === --

type family RecordOf2 t

class HasRecord2 t where
    record2 :: Lens' t (RecordOf2 t)


-- type instance RecordOf2 (Term attrs tags) = Access ExprData (Term attrs tags)

-- instance Accessible' ExprData (Term attrs tags)
--       => HasRecord2 (Term attrs tags) where record2 = List.access' ExprData

-- -- TODO [WD]: Add TH case' interface
-- __case__ lib file loc = fromJustNote err ∘∘ runMatches where
--     err = lib <> ": " <> file <> ":" <> show loc <> ": Non-exhaustive patterns in case"
-- {-# INLINE __case__ #-}

-- === Pattern Matching === --


newtype MatchState3 rec out a = MatchState3 {ffg :: forall s. StateT (MVector s (rec -> out)) (ST s) a }
type    MatchSet3   rec out  = MatchState3 rec out ()


instance Functor (MatchState3 rec out) where
    fmap f (MatchState3 s) = MatchState3 $ f <$> s ; {-# INLINE fmap #-}

instance Applicative (MatchState3 rec out) where
    pure a = MatchState3 $ pure a ; {-# INLINE pure #-}
    (MatchState3 f) <*> (MatchState3 a) = MatchState3 $ f <*> a

instance Monad (MatchState3 rec out) where
    return a = MatchState3 $ return a
    (MatchState3 a) >>= f = MatchState3 $ a >>= (fmap ffg f)


class Match3 v rec where
    of3 :: forall out. (v -> out) -> MatchSet3 rec out


-- FIXME: draft implementation, to refactor
instance ( KnownNat (FromJust (Encode2 Atom atom))
         , Phantom atom
         , bind ~ bind'
         , dyn  ~ dyn' )
      => Match3 (Symbol.Data atom dyn bind) (ExprRecord2 bind' model (Layout dyn' layout)) where
    of3 f = MatchState3 $ do
        reg  <- get
        -- let run = error "x"
        -- let run (ExprRecord2 (d@(Data3 _ store))) = f $ unsafeRestore store
        let run (ExprRecord2 d) = f $ unsafeCoerce sym where
                Raw sym = Prop.get @Symbol d

        V.unsafeWrite reg (encode3 @Atom (phantom :: atom)) run
    {-# INLINE of3 #-}

-- case3 = error "y"
--
        -- case3 :: ExprRecord2 bind model scope ~ rec => rec -> MatchSet3 rec out -> out
        -- case3 rec@(ExprRecord2 (d@(Data3 nat _))) (MatchState3 body) = runST $ do
        --     defaults  <- V.replicate (fromIntegral $ natVal (Proxy :: Proxy (Size PossibleVariants))) (error "Non-exhaustive patterns in case")
        --     selectors <- execStateT body defaults
        --     func      <- V.unsafeRead selectors nat
        --     return $ func rec
        -- {-# INLINE case3 #-}


case3 :: ExprRecord2 bind model scope ~ rec => rec -> MatchSet3 rec out -> out
-- case3 :: ExprRecord2 bind model scope ~ rec => rec -> MatchSet3 rec out -> IO ()
case3 rec@(ExprRecord2 d) (MatchState3 body) = runST $ do
    let Enum nat = Prop.get @Atom d
    defaults  <- V.replicate (fromIntegral $ natVal (Proxy :: Proxy (Size PossibleVariants))) (error "Non-exhaustive patterns in case")
    selectors <- execStateT body defaults
    func      <- V.unsafeRead selectors nat
    return $ func rec
    -- return (return ())
{-# INLINE case3 #-}


----------------------------------------------------



-- Term '[Scope := Draft, Dynamics := Static, Layout := Network] '[Expr, Type, ...]
-- Layout okresla jakie sa polaczenia pomiedzy elementami AST/ASG oraz dla innych warstw jak np. dla Type.
-- UWAGA: dla nodow sa inne polaczenia niz dla Type - dla Type sa edge, dla nodow nie? Czy moze sa takie same?







--







-- !!!!!!!!!!!!!!!!!!!!!! przy konstruktorach robimy tak ze atom wkladamy do monady konstruujacej i odpalamy tworzenie warst. Te ktore beda go chcialy sie do niego dostana. Wtedy bedziemy wiedzieli jak uzyc dokladnie VGRecord2


newtype     Expr        t fmt dyn sel = Expr (Layout_OLD t fmt dyn sel)
type        AnyExpr     t fmt dyn     = Expr         t fmt dyn 'Nothing
type        LimitedExpr t fmt dyn a   = Expr         t fmt dyn ('Just a)
type        AtomicExpr  t fmt dyn a   = LimitedExpr  t fmt dyn '[a]

type family Layout_OLD      t fmt dyn (sel :: Maybe [*]) :: *
-- type family Layout2     t a :: *
type family TermOf      a

type family Layout2 t :: * -> *

-- === Utils === --

type family Selected (sel :: Maybe [*]) (lst :: [*]) where
            Selected 'Nothing    lst = lst
            Selected ('Just sel) lst = sel -- FIXME[WD]: The selection does NOT check if it matches with possible candidates

type        Variants        t fmt  dyn a bind = Symbol.Datas (Selected a (Atoms fmt)) dyn bind
type        SubDynExprs     t fmt  dyn        = Expr t fmt <$> SubDynamics     dyn <*> '[ 'Nothing ]
type        SubSemiDynExprs t fmt  dyn        = Expr t fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ]
type        SubExprs        t fmt  dyn        = SubExprs' t (SubFormats fmt) dyn
type family SubExprs'       t fmts dyn where
            SubExprs' t '[]           dyn = '[]
            SubExprs' t '[fmt]        dyn = SubDynExprs     t fmt dyn
            SubExprs' t (fmt ': fmts) dyn = SubSemiDynExprs t fmt dyn <> SubExprs' t fmts dyn


type        Variants2        fmt  dyn sel a = Symbol.Datas (Selected sel (Atoms fmt)) dyn a


-- type        SubDynExprs2     fmt  dyn a      = Expr2 fmt <$> SubDynamics     dyn <*> '[ 'Nothing ] <*> '[ a ]
-- type        SubSemiDynExprs2 fmt  dyn a      = Expr2 fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ] <*> '[ a ]
-- type        SubExprs2        fmt  dyn a      = SubExprs2' (SubFormats fmt) dyn a
-- type family SubExprs2' fmts          dyn a where
--             SubExprs2' '[]           dyn a = '[]
--             SubExprs2' '[fmt]        dyn a = SubDynExprs2     fmt dyn a
--             SubExprs2' (fmt ': fmts) dyn a = SubSemiDynExprs2 fmt dyn a <> SubExprs2' fmts dyn a


-- === Defaults === --

-- | Standard expr record definition
type ExprRecord t fmt dyn sel a = VGRecord2 (SubExprs t fmt dyn) (Variants t fmt dyn sel a) Data2
-- newtype ExprRecord2 fmt dyn sel a = ExprRecord2 (VGRecord2 (SubExprs2 fmt dyn a) (Variants2 fmt dyn sel a) Data2)


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Expr t fmt dyn sel)) => Show (Expr t fmt dyn sel)

-- Relations
type instance TermOf   (Expr t fmt dyn sel) = Expr t fmt dyn sel
type instance Base     (Expr t fmt dyn sel) = fmt
type instance RecordOf (Expr t fmt dyn sel) = RecordOf (Unwrapped (Expr t fmt dyn sel))

-- Wrapper
makeWrapped ''Expr

-- Record
instance IsRecord  (Unwrapped (Expr t fmt dyn a)) => IsRecord  (Expr t fmt dyn a) where asRecord = wrapped' ∘ asRecord ; {-^. INLINE asRecord ^.-}
instance HasRecord (Unwrapped (Expr t fmt dyn a)) => HasRecord (Expr t fmt dyn a) where record   = wrapped' ∘ record   ; {-^. INLINE record   #-}

-- Shell
instance Shell.HasLayer' l (Unwrapped (Expr t fmt dyn sel)) => Shell.HasLayer' l (Expr t fmt dyn sel) where
    layer' = wrapped' ∘ layer' ; {-# INLINE layer' #-}



-------------------------
-- === OverBuilder === --
-------------------------

class Monad m => OverBuilder m a where
    overbuild :: RecordOf a -> m a

instance Monad m => OverBuilder m (VGRecord2 gs vs d) where
    overbuild = return ; {-# INLINE overbuild #-}

instance (Monad m, OverBuilder m (Unwrapped (Expr t fmt dyn a))) => OverBuilder m (Expr t fmt dyn a) where
    overbuild = Expr <∘> overbuild ; {-# INLINE overbuild #-}



-------------------------------------
-- === Term Layout type caches === --
-------------------------------------

-- TODO: Refactor to Possible type class and arguments Variants etc.
-- type PossibleElements = [Static, Dynamic, Literal, Value, Thunk, Phrase, Draft, Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type OffsetVariants = 7

type instance Encode rec (Symbol.Data atom dyn a) = {-dyn-} 0 ': Decode rec atom ': {-formats-} '[6]


type instance Decode rec Static  = 0
type instance Decode rec Dynamic = 1

type instance Decode rec Literal = 2
type instance Decode rec Value   = 3
type instance Decode rec Thunk   = 4
type instance Decode rec Phrase  = 5
type instance Decode rec Draft   = 6

type instance Decode rec Acc     = 7
type instance Decode rec App     = 8
type instance Decode rec Blank   = 9
type instance Decode rec Cons    = 10
type instance Decode rec Curry   = 11
type instance Decode rec Lam     = 12
type instance Decode rec Match   = 13
type instance Decode rec Missing = 14
type instance Decode rec Native  = 15
type instance Decode rec Star    = 16
type instance Decode rec Unify   = 17
type instance Decode rec Var     = 18


type instance Encode2 Atom    v = Index v PossibleVariants
type instance Encode2 Format  v = Index v PossibleFormats


-- TODO: refactor, Decode2 should replace Decode. Refactor Decode -> Test
-- TODO: Decode2 should have more params to be more generic
type family Decode2 (ns :: [Nat]) :: [*] where
    Decode2 '[] = '[]
    Decode2 (n ': ns) = DecodeComponent n ': Decode2 ns

type family DecodeComponent (n :: Nat) :: *

type instance DecodeComponent 0 = Static
type instance DecodeComponent 1 = Dynamic

type instance DecodeComponent 2 = Literal
type instance DecodeComponent 3 = Value
type instance DecodeComponent 4 = Thunk
type instance DecodeComponent 5 = Phrase
type instance DecodeComponent 6 = Draft

type instance DecodeComponent 7  = Acc
type instance DecodeComponent 8  = App
type instance DecodeComponent 9  = Blank
type instance DecodeComponent 10 = Cons
type instance DecodeComponent 11 = Curry
type instance DecodeComponent 12 = Lam
type instance DecodeComponent 13 = Match
type instance DecodeComponent 14 = Missing
type instance DecodeComponent 15 = Native
type instance DecodeComponent 16 = Star
type instance DecodeComponent 17 = Unify
type instance DecodeComponent 18 = Var



-- class RecordRepr rec where
--     recordRepr :: rec -> String
--
-- instance RecordRepr

-- class Cons2 v t where
--     cons2 :: v -> t
