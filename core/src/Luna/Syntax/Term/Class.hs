{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Luna.Syntax.Term.Class where


import           Prelude.Luna                 hiding (Num, Swapped, Curry)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, WithDynamics, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import qualified Data.Reprx                   as Repr
import           Type.Bool
import           Luna.Syntax.Term.Format
import qualified Luna.Syntax.Term.Lit     as Lit
import Luna.Syntax.Term.Atom

import Data.Shell               as Shell
import Data.Record.Model.Masked as X (Data, Data2, TermRecord, VGRecord2)
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)
import Luna.Syntax.Term.Expr (Expr, Exprs, NameByDynamics)








--

--type family TakeUntil (a :: k) (ls :: [k]) :: [k] where
type family TakeUntil a ls where
    TakeUntil a '[]       = '[]
    TakeUntil a (a ': ls) = '[a]
    TakeUntil a (l ': ls) = l ': TakeUntil a ls




--

type SubFormats a = TakeUntil a Formats

--

type ExprRecord gs vs t = TermRecord gs vs t




-- === Refactor === --


-- newtype     Expr      t fmt dyn = Expr (ExprRecord (SubExprs t fmt dyn) (Variants2 t fmt dyn) t) deriving (Generic, NFData, Show)
-- type        Variants2 t fmt dyn = Atoms (Elems fmt) dyn (Layout t fmt dyn)





type family LayoutType a
-- type family ExprOf a

type family   Elems t      :: [*]
type instance Elems Lit    = '[Lit.Star, Lit.String, Lit.Number          ]
type instance Elems Val    = '[Cons    , Lam                             ] <> Elems Lit
type instance Elems Thunk  = '[Acc     , App       , Curry      , Native ] <> Elems Val
type instance Elems Phrase = '[Var     , Unify     , Match               ] <> Elems Thunk
type instance Elems Draft  = '[Blank                                     ] <> Elems Phrase






-----------------------
-- === Selectors === --
-----------------------

type family Selected (sel :: Maybe [*]) (lst :: [*]) where
            Selected 'Nothing    lst = lst
            Selected ('Just sel) lst = sel -- FIXME[WD]: Selekcja nie jest sprawdzana czy matchuje sie z mozlwymi wyborami


-------------------
-- === Terms === --
-------------------

-- === Definitions === --

newtype     Term2       t fmt dyn sel = Term2 (Layout2 t fmt dyn sel)
type        AnyTerm     t fmt dyn     = Term2       t fmt dyn 'Nothing
type        LimitedTerm t fmt dyn a   = Term2       t fmt dyn ('Just a)
type        KnownTerm   t fmt dyn a   = LimitedTerm t fmt dyn '[a]

type family Layout2     t fmt dyn (sel :: Maybe [*]) :: *
type family TermOf      a


-- === Utils === --


type        Variants3       t fmt  dyn a bind = Exprs (Selected a (Elems fmt)) dyn bind
type        SubDynExprs     t fmt  dyn        = Term2 t fmt <$> SubDynamics     dyn <*> '[ 'Nothing ]
type        SubSemiDynExprs t fmt  dyn        = Term2 t fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ]
type        SubExprs        t fmt  dyn        = SubExprs' t (SubFormats fmt) dyn
type family SubExprs'       t fmts dyn where
            SubExprs' t '[]           dyn = '[]
            SubExprs' t '[fmt]        dyn = SubDynExprs     t fmt dyn
            SubExprs' t (fmt ': fmts) dyn = SubSemiDynExprs t fmt dyn <> SubExprs' t fmts dyn


-- === Defaults === --

-- | Standard term record definition
type TermRecord2 t fmt dyn a bind = VGRecord2 (SubExprs t fmt dyn) (Variants3 t fmt dyn a bind) Data2


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Term2 t fmt dyn sel)) => Show (Term2 t fmt dyn sel)

-- Relations
type instance TermOf   (Term2 t fmt dyn sel) = Term2 t fmt dyn sel
type instance Base     (Term2 t fmt dyn sel) = fmt
type instance RecordOf (Term2 t fmt dyn sel) = RecordOf (Unwrapped (Term2 t fmt dyn sel))

-- Wrapper
makeWrapped ''Term2

-- Record
instance IsRecord  (Unwrapped (Term2 t fmt dyn a)) => IsRecord  (Term2 t fmt dyn a) where asRecord = wrapped' ∘ asRecord ; {-# INLINE asRecord #-}
instance HasRecord (Unwrapped (Term2 t fmt dyn a)) => HasRecord (Term2 t fmt dyn a) where record   = wrapped' ∘ record   ; {-# INLINE record   #-}

-- Shell
instance Shell.HasLayer' l (Unwrapped (Term2 t fmt dyn sel)) => Shell.HasLayer' l (Term2 t fmt dyn sel) where
    layer' = wrapped' ∘ layer' ; {-# INLINE layer' #-}



-------------------
-- === Shell === --
-------------------

class Monad m => OverBuilder m a where
    overbuild :: RecordOf a -> m a

instance Monad m => OverBuilder m (VGRecord2 gs vs d) where
    overbuild = return ; {-# INLINE overbuild #-}

instance OverBuilder m (Unwrapped (Term2 t fmt dyn a)) => OverBuilder m (Term2 t fmt dyn a) where
    overbuild = Term2 <∘> overbuild ; {-# INLINE overbuild #-}




-------------------------------------
-- === Term Layout type caches === --
-------------------------------------

type instance Encode (Expr Blank dyn a) rec = '[ 41 , 7,8               ]
type instance Decode (Expr Blank dyn a) rec = 41
