{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}

module Luna.Syntax.Term.Expr.Class where


import           Prelude.Luna                 hiding (Num, Swapped, Curry, String, Integer, Rational)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons, Value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import qualified Data.Reprx                   as Repr
import           Type.Bool
import           Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Symbol

import Data.Shell               as Shell
import Data.Record.Model.Masked as X (Data, Data2, TermRecord, VGRecord2)
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)
import Luna.Syntax.Term.Expr (Atom, Atoms, NameByDynamics)


import Data.Container.Hetero (Elems)
import Data.RTuple (Empty, empty)




data Scope = Scope deriving (Show)
data Layout = Layout deriving (Show)
data Binding = Binding deriving (Show)
type family Bound layout (attrs :: [*]) :: * -- to musi byc tak zadeklarowane, poniewaz w ``(Binding (term # Layout) term)`, `term` moze byc zbyt rozpakowanego typu


type family (a :: k) ^. (prop :: *) :: *
data a := b


-------------------
-- === Terms === --
-------------------

-- === Definitions === --

newtype Term (attrs :: [*]) layers = Term (TermStack attrs layers)

type TermStack     (attrs :: [*]) layers = Stack layers (TermLayerDesc attrs <$> layers)
data TermLayerDesc (attrs :: [*]) layer
type TermLayer     (attrs :: [*]) layer  = Layer (TermLayerDesc attrs layer)
type TermLayers    (attrs :: [*]) layers = Layer <$> (TermLayerDesc attrs <$> layers)

-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Term attrs layers)) => Show (Term attrs layers)

-- Wrapper
makeWrapped ''Term

-- Construction
instance layers ~ '[] => Empty (Term attrs layers) where
    empty = Term empty ; {-# INLINE empty #-}


------------------
-- === Expr === --
------------------

-- === Definitions === --

type Expr2 binding layers dyn layout = Term '[Binding := binding, Layout := layout, Dynamics := dyn] (ExprData ': layers)


-- TO REFACTOR:
type instance ((k := v) ': ls) ^. l = If (k == l) v (ls ^. l)

-- type Expr2 binding layers layout = Term '[Binding := binding, Layout := layout] (ExprData ': layers)
--
-- type Expr3 binding layers layout scope = Term '[Binding := binding, Layout := layout, Scope := scope] (ExprData ': layers)
--
-- Expr3 Network '[] (Layout Static Draft) (Layout Static Draft)
-- Expr3 Network '[] (Layout Static Draft) (Layout Static App)
--
--
-- Node Static Draft Static App
--
-- Node' Static Draft
--
--
-- Scoped Node Static Draft App


-- === Expr layer === --

data ExprData = ExprData deriving (Show)
type instance LayerData (TermLayerDesc attrs ExprData) = ExprRecord2 (Bound (attrs ^. Binding) attrs) (attrs ^. Dynamics) (attrs ^. Layout)
type instance LayerData (TermLayerDesc attrs Int) = Int

-- === ExprRecord === --

-- newtype ExprRecord2 layout dyn bind = ExprRecord2 (VGRecord2 '[] (Atoms (Elems layout) dyn bind) Data2)
newtype ExprRecord2 bind dyn layout = ExprRecord2 Data2 deriving (Show)




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

type        Variants        t fmt  dyn a bind = Atoms (Selected a (Elems fmt)) dyn bind
type        SubDynExprs     t fmt  dyn        = Expr t fmt <$> SubDynamics     dyn <*> '[ 'Nothing ]
type        SubSemiDynExprs t fmt  dyn        = Expr t fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ]
type        SubExprs        t fmt  dyn        = SubExprs' t (SubFormats fmt) dyn
type family SubExprs'       t fmts dyn where
            SubExprs' t '[]           dyn = '[]
            SubExprs' t '[fmt]        dyn = SubDynExprs     t fmt dyn
            SubExprs' t (fmt ': fmts) dyn = SubSemiDynExprs t fmt dyn <> SubExprs' t fmts dyn


type        Variants2        fmt  dyn sel a = Atoms (Selected sel (Elems fmt)) dyn a


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

type instance Encode rec (Atom symbol dyn a) = {-dyn-} 0 ': Decode rec symbol ': {-formats-} '[6]


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


-- class Cons2 v t where
--     cons2 :: v -> t
