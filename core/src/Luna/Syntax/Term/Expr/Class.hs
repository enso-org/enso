{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Luna.Syntax.Term.Expr.Class where


import           Prelude.Luna                 hiding (Num, Swapped, Curry, String, Integer, Rational)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons, Value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container
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



-------------------
-- === Terms === --
-------------------

-- === Definitions === --

-- newtype     Expr2        fmt dyn sel a = Expr2 (ExprRecord2 fmt dyn sel a)

-- AST to zla nazwa, to powinien byc symbol terminalny lub cos podobnego - to cos co ma warstwy, choc nie do konca, bo jest to tylko transformata
-- powinnismy ponadto zamiast :| zastosowac mapowanie ls jak w Mainie: TermShell
-- tylko nalezy zmienic definicje shella z
-- newtype Shell    ls = Shell (TMap (ls :->> Layers ls))
-- na
-- newtype Shell ks ls = Shell (TMap (ks :->> Layers ls))
-- i przemapowac sie ladniej, dzieki temu nie trzeba definiowac przeksztalcen proxy jak w mainie (type instance Shell.Access ...)
-- newtype AST t f = AST (f (Bind t (AST t f)))
-- TODO ^^^
-- newtype AST t ls f = AST (ls :| f (Item (AST t ls f)))
--
-- type ASTExpr t ls fmt dyn sel = AST t ls (ExprRecord2 fmt dyn sel)
newtype ExprLayer t a = ExprLayer a deriving (Show, Functor, Traversable, Foldable)

type TermStack ls el = Stack ls (ExprLayer el <$> ls)

newtype Element ls = Element (TermStack ls (Element ls))

data Expr2 = Expr2 deriving (Show)

type instance LayerData (ExprLayer el Expr2) = ExprRecord2 (el # Format) (el # Dynamics) 'Nothing el

type family a # prop

newtype     Expr        t fmt dyn sel = Expr (Layout t fmt dyn sel)
type        AnyExpr     t fmt dyn     = Expr         t fmt dyn 'Nothing
type        LimitedExpr t fmt dyn a   = Expr         t fmt dyn ('Just a)
type        AtomicExpr  t fmt dyn a   = LimitedExpr  t fmt dyn '[a]

type family Layout      t fmt dyn (sel :: Maybe [*]) :: *
-- type family Layout2     t a :: *
type family TermOf      a


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
newtype ExprRecord2 fmt dyn sel a = ExprRecord2 (VGRecord2 '[] (Variants2 fmt dyn sel a) Data2)


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
instance IsRecord  (Unwrapped (Expr t fmt dyn a)) => IsRecord  (Expr t fmt dyn a) where asRecord = wrapped' ∘ asRecord ; {-# INLINE asRecord #-}
instance HasRecord (Unwrapped (Expr t fmt dyn a)) => HasRecord (Expr t fmt dyn a) where record   = wrapped' ∘ record   ; {-# INLINE record   #-}

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

instance OverBuilder m (Unwrapped (Expr t fmt dyn a)) => OverBuilder m (Expr t fmt dyn a) where
    overbuild = Expr <∘> overbuild ; {-# INLINE overbuild #-}



-------------------------------------
-- === Term Layout type caches === --
-------------------------------------

type instance Encode (Atom Blank dyn a) rec = '[ 41 , 7,8               ]
type instance Decode (Atom Blank dyn a) rec = 41
-- type instance Encode (Expr Unify dyn a) rec = '[ 41 , 7,8               ]
