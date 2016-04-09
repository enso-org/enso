{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Luna.Syntax.Term.Expr (module Luna.Syntax.Term.Expr, module X) where


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
import Luna.Syntax.Term.Atom as X

import Data.Shell
import Data.Record.Model.Masked as X (Data, Data2, TermRecord, VGRecord2)
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)

undefined = error "oh no in Expr.hs"




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
type instance Elems Lit    = '[Lit.Star, Lit.String, Lit.Number           ]
type instance Elems Val    = '[Cons'   , Lam'                             ] <> Elems Lit
type instance Elems Thunk  = '[Acc'    , App'      , Curry'     , Native' ] <> Elems Val
type instance Elems Phrase = '[Var'    , Unify'    , Match'               ] <> Elems Thunk
type instance Elems Draft  = '[Blank'                                     ] <> Elems Phrase






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


type        Variants3       t fmt  dyn a bind = Atoms (Selected a (Elems fmt)) dyn bind
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

-- IsRecord
instance IsRecord (Unwrapped (Term2 t fmt dyn a)) => IsRecord (Term2 t fmt dyn a) where
    asRecord = wrapped' ∘ asRecord ; {-# INLINE asRecord #-}





-------------------
-- === Shell === --
-------------------

class Monad m => OverBuilder m a where
    overbuild :: RecordOf a -> m a

instance Monad m => OverBuilder m (VGRecord2 gs vs d) where
    overbuild = return ; {-# INLINE overbuild #-}

instance OverBuilder m (Unwrapped (Term2 t fmt dyn a)) => OverBuilder m (Term2 t fmt dyn a) where
    overbuild = Term2 <∘> overbuild ; {-# INLINE overbuild #-}



-- barr = () where
--     a = undefined :: Proxy Foox
--     b = a :: Proxy '[]


-- barr2 = () where
--     a = undefined :: Proxy (In (Atom Blank' Static Int) Foox)
--     b = a :: Proxy 'False


-- === Utils === --



-- === Instances === --

-- Basic instances
-- type instance Dynamics   (Expr t fmt dyn) = dyn
-- type instance LayoutType (Expr t fmt dyn) = t
-- -- type instance ExprOf     (Expr t fmt dyn) = Expr t fmt dyn
--
-- -- instance Eq  (Expr t fmt dyn) where (==)    = $notImplemented
-- -- instance Ord (Expr t fmt dyn) where compare = $notImplemented
--
-- -- Wrappers & Layers
-- -- makeWrapped ''Expr
-- --TODO[WD]: add makeLayered util
-- type instance Unlayered (Expr t fmt dyn) = Unwrapped (Expr t fmt dyn)
-- instance      Layered   (Expr t fmt dyn)
--
-- -- Record instances
-- type instance RecordOf (Expr t fmt dyn) = RecordOf (Unlayered (Expr t fmt dyn))
-- instance IsRecord (Unlayered (Expr t fmt dyn)) => IsRecord (Expr t fmt dyn) where asRecord = wrapped' ∘ asRecord ; {-# INLINE asRecord #-}
--
-- -- Dynamics
-- type instance WithDynamics dyn' (Expr t fmt dyn) = Expr t fmt dyn'
--
-- -- Properties
-- type instance Props p (Expr t fmt dyn) = Props p (RecordOf (Expr t fmt dyn))
--
-- -- Conversions
-- instance Unwrapped (Expr t fmt dyn) ~ ExprRecord gs vs t' => Convertible (Expr t fmt dyn) (ExprRecord gs vs t') where convert = unwrap' ; {-# INLINE convert #-}
--
-- instance Convertible (Unwrapped (Expr t fmt dyn)) Data => Castable    (Expr t fmt dyn) Data
-- instance Convertible (Unwrapped (Expr t fmt dyn)) Data => Convertible (Expr t fmt dyn) Data where convert = convert ∘ unwrap' ; {-# INLINE convert #-}
-- instance Castable    Data (Unwrapped (Expr t fmt dyn)) => Castable    Data (Expr t fmt dyn) where cast    = wrap'   ∘ cast    ; {-# INLINE cast    #-}
--
-- -- Abstractions
-- type instance                                                       Abstract    (Expr t fmt dyn) = Data
-- instance BiCastable (Abstract (Expr t fmt dyn)) (Expr t fmt dyn) => IsAbstract  (Expr t fmt dyn) where abstracted = iso cast cast ; {-# INLINE abstracted #-}
-- instance BiCastable (Abstract (Expr t fmt dyn)) (Expr t fmt dyn) => HasAbstract (Expr t fmt dyn)






-----------------------------
-- === Term definition === --
-----------------------------

-- | The following definitions are parameterized by the `t` type, which indicates which data `Layout` to choose.
--   The `Layout` type family defines the recursive layout for AST structures.

newtype     Term     t term rt = Term (TermRecord (SubTerms t term rt) (Variants t term rt) t) deriving (Generic, NFData, Show)
type        Variants t term rt = Elems_OLD term (NameByDynamics rt (Layout t term rt)) (Layout t term rt)
type family Layout    t fmt dyn


type family Input     a
type family NameInput a where
    NameInput I = Impossible
    NameInput a = If (Dynamics a == Static) Lit.String (Input a)


-- === Elems_OLD === --

type family   Elems_OLD term  n t :: [*]

type instance Elems_OLD Lit    n t = Lit.Star
                              ': Lit.String
                              ': Lit.Number
                              ': '[]

type instance Elems_OLD Val    n t = Cons         n t
                              ': Lam            t
                              ': Elems_OLD Lit    n t

type instance Elems_OLD Thunk  n t = Acc          n t
                              ': App            t
                              ': Curry          t
                              ': Native       n
                              ': Elems_OLD Val    n t

type instance Elems_OLD Phrase n t = Var          n
                              ': Unify          t
                              ': Match          t
                              ': Elems_OLD Thunk  n t

type instance Elems_OLD Draft  n t = Blank
                              ': Elems_OLD Phrase n t



-- === Syntax Layouts === --


type SubDynTerms     t fmt dyn = Term t fmt <$> SubDynamics     dyn
type SubSemiDynTerms t fmt dyn = Term t fmt <$> SubSemiDynamics dyn


type SubTerms t fmt dyn = SubTerms' t dyn (SubFormats fmt)
type family SubTerms' dyn t fmts where
  SubTerms' t dyn '[]           = '[]
  SubTerms' t dyn '[fmt]        = SubDynTerms     t fmt dyn
  SubTerms' t dyn (fmt ': fmts) = SubSemiDynTerms t fmt dyn <> SubTerms' t dyn fmts


--







-- === Variant repr === --

type VariantRepr s rec = WithElement' ElemShow rec (Repr.Builder s Repr.Tok)

class                                                 ElemShow a out where elemShow :: a -> out
instance (Repr s a, Repr.Builder s Repr.Tok ~ out) => ElemShow a out where elemShow = repr

instance {-# OVERLAPPABLE #-}  VariantRepr s (TermRecord gs vs t)                         => Repr s          (TermRecord gs vs t)  where repr   = variantRepr                                      ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} (VariantRepr s (Unwrapped (Term t term rt)), Typeable term) => Repr s          (Term      t term rt) where repr t = fromString (showTermType t) <+> repr (unwrap' t) ; {-# INLINE repr #-}
instance                       VariantRepr HeaderOnly (Unwrapped (Term t term rt))        => Repr HeaderOnly (Term      t term rt) where repr   = repr ∘ unwrap'                                   ; {-# INLINE repr #-}

variantRepr :: VariantRepr s rec => rec -> Repr.Builder s Repr.Tok
variantRepr = withElement' (Proxy :: Proxy ElemShow) elemShow


-- === Utils === --

showTermType :: Typeable term => Term t term rt -> String
showTermType (t :: Term t term rt) = tyConName $ typeRepTyCon $ head $ snd $ splitTyConApp $ typeOf (Proxy :: Proxy term)


-- === Instances === --

-- Basic instances
type instance Dynamics   (Term t term rt) = rt
type instance LayoutType (Term t term rt) = t
type instance TermOf     (Term t term rt) = Term t term rt

instance Eq  (Term t term rt) where (==)    = $notImplemented
instance Ord (Term t term rt) where compare = $notImplemented

-- Bases
type instance Base (Term t term rt) = Proxy term

-- Wrappers & Layers
makeWrapped ''Term
type instance Unlayered (Term t term rt) = Unwrapped (Term t term rt)
instance      Layered   (Term t term rt)

-- Record instances
type instance RecordOf (Term t term rt) = RecordOf (Unlayered (Term t term rt))
instance IsRecord (Unlayered (Term t term rt)) => IsRecord (Term t term rt) where asRecord = wrapped' ∘ asRecord

-- Layouts
type instance WithDynamics rt' (Term t term rt) = Term t term rt'

-- Properties
type instance Props p (Term t term rt) = Props p (RecordOf (Term t term rt))

-- Conversions
instance Unwrapped (Term t term rt) ~ TermRecord gs vs t' => Convertible (Term t term rt) (TermRecord gs vs t') where convert = unwrap' ; {-# INLINE convert #-}

instance Convertible (Unwrapped (Term t term rt)) Data => Castable    (Term t term rt) Data
instance Convertible (Unwrapped (Term t term rt)) Data => Convertible (Term t term rt) Data where convert = convert ∘ unwrap' ; {-# INLINE convert #-}
instance Castable    Data (Unwrapped (Term t term rt)) => Castable    Data (Term t term rt) where cast    = wrap'   ∘ cast    ; {-# INLINE cast    #-}


-- Abstractions
type instance                                                       Abstract    (Term t term rt) = Data
instance BiCastable (Abstract (Term t term rt)) (Term t term rt) => IsAbstract  (Term t term rt) where abstracted = iso cast cast
instance BiCastable (Abstract (Term t term rt)) (Term t term rt) => HasAbstract (Term t term rt)



-------------------------------------
-- === Term Layout type caches === --
-------------------------------------

-- | Options in this section should be used only for development purpose and should never be enabled in production ready code.
-- | Their behaviour bases often on manually cached code, which could accidentaly get obsolete.
-- | We could probably throw it away in the future, but the following GHC bugs have to be resolved first:
-- |    - https://ghc.haskell.org/trac/ghc/ticket/8095
-- |    - https://ghc.haskell.org/trac/ghc/ticket/11375

#ifndef RELEASE
#ifdef  FastCompilation
#define CachedTypeFamilies
#endif
#endif

-- Cache related pragmas
#define CACHE(n)       cacheHelper ''n Nothing  ; cacheType ''n Nothing
#define CACHE_AS(n,cn) cacheHelper ''n (Just cn); cacheType ''n (Just cn)
#define CHECK_EQ(s,t)  assertTypesEq (Proxy :: Proxy (s)) (Proxy :: Proxy (t))


-- === Definitions === --

-- The following code is result of type-families expressions and is cached in order to speed-up the compilation process.
-- related GHC bug:        https://ghc.haskell.org/trac/ghc/ticket/8095#no1
-- related IRC discussion: http://pastebin.com/9PH7TPB9

-- | All possible groups and variants stored as single 64-bit mask:
-- |   - 9  bits for groups
-- |   - 36 bits for variants
-- |   - 19 bits free for further extensions

-- === VariantList === --

type  GroupList t =              '[ {-  0 -} Term t Lit   Static
                                  , {-  1 -} Term t Val   Static
                                  , {-  2 -} Term t Val   Dynamic
                                  , {-  3 -} Term t Thunk Static
                                  , {-  4 -} Term t Thunk Dynamic
                                  , {-  5 -} Term t Phrase Static
                                  , {-  6 -} Term t Phrase Dynamic
                                  , {-  7 -} Term t Draft Static
                                  , {-  8 -} Term t Draft Dynamic
                                  ]
type VariantList_MANUAL_CACHE t = [ {-  9 -} Lit.Star
                                  , {- 10 -} Lit.String
                                  , {- 11 -} Lit.Number
                                  , {- 12 -} Cons   Lit.String (Layout t Val   Static )
                                  , {- 13 -} Lam               (Layout t Val   Static )
                                  , {- 14 -} Cons              (Layout t Val   Dynamic) (Layout t Val Dynamic)
                                  , {- 15 -} Lam               (Layout t Val   Dynamic)
                                  , {- 16 -} Cons   Lit.String (Layout t Thunk Static )
                                  , {- 17 -} Acc    Lit.String (Layout t Thunk Static )
                                  , {- 18 -} App               (Layout t Thunk Static )
                                  , {- 19 -} Curry             (Layout t Thunk Static )
                                  , {- 20 -} Lam               (Layout t Thunk Static )
                                  , {- 21 -} Native Lit.String
                                  , {- 22 -} Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)
                                  , {- 23 -} App               (Layout t Thunk Dynamic)
                                  , {- 24 -} Curry             (Layout t Thunk Dynamic)
                                  , {- 25 -} Cons              (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)
                                  , {- 26 -} Lam               (Layout t Thunk Dynamic)
                                  , {- 27 -} Native            (Layout t Thunk Dynamic)
                                  , {- 28 -} Var    Lit.String
                                  , {- 29 -} Cons   Lit.String (Layout t Phrase Static )
                                  , {- 30 -} Unify             (Layout t Phrase Static )
                                  , {- 31 -} Match             (Layout t Phrase Static )
                                  , {- 32 -} Acc    Lit.String (Layout t Phrase Static )
                                  , {- 33 -} App               (Layout t Phrase Static )
                                  , {- 34 -} Curry             (Layout t Phrase Static )
                                  , {- 35 -} Lam               (Layout t Phrase Static )
                                  , {- 36 -} Var               (Layout t Phrase Dynamic)
                                  , {- 37 -} Unify             (Layout t Phrase Dynamic)
                                  , {- 38 -} Match             (Layout t Phrase Dynamic)
                                  , {- 39 -} Acc               (Layout t Phrase Dynamic) (Layout t Phrase Dynamic)
                                  , {- 40 -} App               (Layout t Phrase Dynamic)
                                  , {- 41 -} Curry             (Layout t Phrase Dynamic)
                                  , {- 42 -} Cons              (Layout t Phrase Dynamic) (Layout t Phrase Dynamic)
                                  , {- 43 -} Lam               (Layout t Phrase Dynamic)
                                  , {- 44 -} Native            (Layout t Phrase Dynamic)
                                  , {- 45 -} Blank
                                  , {- 46 -} Cons   Lit.String (Layout t Draft Static )
                                  , {- 47 -} Unify             (Layout t Draft Static )
                                  , {- 48 -} Match             (Layout t Draft Static )
                                  , {- 49 -} Acc    Lit.String (Layout t Draft Static )
                                  , {- 50 -} App               (Layout t Draft Static )
                                  , {- 51 -} Curry             (Layout t Draft Static )
                                  , {- 52 -} Lam               (Layout t Draft Static )
                                  , {- 53 -} Var               (Layout t Draft Dynamic)
                                  , {- 54 -} Unify             (Layout t Draft Dynamic)
                                  , {- 55 -} Match             (Layout t Draft Dynamic)
                                  , {- 56 -} Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic)
                                  , {- 57 -} App               (Layout t Draft Dynamic)
                                  , {- 58 -} Curry             (Layout t Draft Dynamic)
                                  , {- 59 -} Cons              (Layout t Draft Dynamic) (Layout t Draft Dynamic)
                                  , {- 60 -} Lam               (Layout t Draft Dynamic)
                                  , {- 61 -} Native            (Layout t Draft Dynamic)
                                  ]

-- #ifndef CachedTypeFamilies

-- FIXME
--type VariantList_RULE t = Unique (GatherProps Variant (GroupList t))
--CACHE_AS(VariantList_RULE, VariantList_GEN_CACHE)
--CHECK_EQ(VariantList_GEN_CACHE IM, VariantList_MANUAL_CACHE IM)
--type VariantList_CACHE t = VariantList_GEN_CACHE t

-- #else

type VariantList_CACHE t = VariantList_MANUAL_CACHE t

-- #endif

type VariantList t = VariantList_CACHE t

-- Layout

type Layout_RULE t = GroupList t <> VariantList t
CACHE_AS(Layout_RULE, "Layout_CACHE")

type instance Record.Layout (TermRecord gs vs t) = Layout_CACHE t

type instance Layout_Variants Variant (TermRecord gs vs t) = VariantList t

-- === DecodeMap === --

type DecodeMap_MANUAL_CACHE t =
    'Map [ {-  0 -} '( Term t Lit   Static                                                   ,  0 )
         , {-  1 -} '( Term t Val   Static                                                   ,  1 )
         , {-  2 -} '( Term t Val   Dynamic                                                  ,  2 )
         , {-  3 -} '( Term t Thunk Static                                                   ,  3 )
         , {-  4 -} '( Term t Thunk Dynamic                                                  ,  4 )
         , {-  5 -} '( Term t Phrase Static                                                  ,  5 )
         , {-  6 -} '( Term t Phrase Dynamic                                                 ,  6 )
         , {-  7 -} '( Term t Draft Static                                                   ,  7 )
         , {-  8 -} '( Term t Draft Dynamic                                                  ,  8 )
         , {-  9 -} '( Lit.Star                                                              ,  9 )
         , {- 10 -} '( Lit.String                                                            , 10 )
         , {- 11 -} '( Lit.Number                                                            , 11 )
         , {- 12 -} '( Cons   Lit.String (Layout t Val   Static )                            , 12 )
         , {- 13 -} '( Lam               (Layout t Val   Static )                            , 13 )
         , {- 14 -} '( Cons              (Layout t Val   Dynamic) (Layout t Val   Dynamic)   , 14 )
         , {- 15 -} '( Lam               (Layout t Val   Dynamic)                            , 15 )
         , {- 16 -} '( Cons   Lit.String (Layout t Thunk Static )                            , 16 )
         , {- 17 -} '( Acc    Lit.String (Layout t Thunk Static )                            , 17 )
         , {- 18 -} '( App               (Layout t Thunk Static )                            , 18 )
         , {- 19 -} '( Curry             (Layout t Thunk Static )                            , 19 )
         , {- 20 -} '( Lam               (Layout t Thunk Static )                            , 20 )
         , {- 21 -} '( Native Lit.String                                                     , 21 )
         , {- 22 -} '( Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)   , 22 )
         , {- 23 -} '( App               (Layout t Thunk Dynamic)                            , 23 )
         , {- 24 -} '( Curry             (Layout t Thunk Dynamic)                            , 24 )
         , {- 25 -} '( Cons              (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)   , 25 )
         , {- 26 -} '( Lam               (Layout t Thunk Dynamic)                            , 26 )
         , {- 27 -} '( Native            (Layout t Thunk Dynamic)                            , 27 )
         , {- 28 -} '( Var    Lit.String                                                     , 28 )
         , {- 29 -} '( Cons   Lit.String (Layout t Phrase Static )                           , 29 )
         , {- 30 -} '( Unify             (Layout t Phrase Static )                           , 30 )
         , {- 31 -} '( Match             (Layout t Phrase Static )                           , 31 )
         , {- 32 -} '( Acc    Lit.String (Layout t Phrase Static )                           , 32 )
         , {- 33 -} '( App               (Layout t Phrase Static )                           , 33 )
         , {- 34 -} '( Curry             (Layout t Phrase Static )                           , 34 )
         , {- 35 -} '( Lam               (Layout t Phrase Static )                           , 35 )
         , {- 36 -} '( Var               (Layout t Phrase Dynamic)                           , 36 )
         , {- 37 -} '( Unify             (Layout t Phrase Dynamic)                           , 37 )
         , {- 38 -} '( Match             (Layout t Phrase Dynamic)                           , 38 )
         , {- 39 -} '( Acc               (Layout t Phrase Dynamic) (Layout t Phrase Dynamic) , 39 )
         , {- 40 -} '( App               (Layout t Phrase Dynamic)                           , 40 )
         , {- 41 -} '( Curry             (Layout t Phrase Dynamic)                           , 41 )
         , {- 42 -} '( Cons              (Layout t Phrase Dynamic) (Layout t Phrase Dynamic) , 42 )
         , {- 43 -} '( Lam               (Layout t Phrase Dynamic)                           , 43 )
         , {- 44 -} '( Native            (Layout t Phrase Dynamic)                           , 44 )
         , {- 45 -} '( Blank                                                                 , 45 )
         , {- 46 -} '( Cons   Lit.String (Layout t Draft Static )                            , 46 )
         , {- 47 -} '( Unify             (Layout t Draft Static )                            , 47 )
         , {- 48 -} '( Match             (Layout t Draft Static )                            , 48 )
         , {- 49 -} '( Acc    Lit.String (Layout t Draft Static )                            , 49 )
         , {- 50 -} '( App               (Layout t Draft Static )                            , 50 )
         , {- 51 -} '( Curry             (Layout t Draft Static )                            , 51 )
         , {- 52 -} '( Lam               (Layout t Draft Static )                            , 52 )
         , {- 53 -} '( Var               (Layout t Draft Dynamic)                            , 53 )
         , {- 54 -} '( Unify             (Layout t Draft Dynamic)                            , 54 )
         , {- 55 -} '( Match             (Layout t Draft Dynamic)                            , 55 )
         , {- 56 -} '( Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic)   , 56 )
         , {- 57 -} '( App               (Layout t Draft Dynamic)                            , 57 )
         , {- 58 -} '( Curry             (Layout t Draft Dynamic)                            , 58 )
         , {- 59 -} '( Cons              (Layout t Draft Dynamic) (Layout t Draft Dynamic)   , 59 )
         , {- 60 -} '( Lam               (Layout t Draft Dynamic)                            , 60 )
         , {- 61 -} '( Native            (Layout t Draft Dynamic)                            , 61 )
         ]

-- #ifndef CachedTypeFamilies

-- FIXME
--type DecodeMap_RULE t = 'Map $ Zip (Layout_CACHE t) (Enumerate (Size (Layout_CACHE t)))
--CACHE_AS(DecodeMap_RULE, DecodeMap_GEN_CACHE)
--CHECK_EQ(DecodeMap_GEN_CACHE IM, DecodeMap_MANUAL_CACHE IM)
--type DecodeMap_CACHE t = DecodeMap_GEN_CACHE t

-- #else

type DecodeMap_CACHE t = DecodeMap_MANUAL_CACHE t

-- #endif

type instance DecodeMap (TermRecord gs vs t) = DecodeMap_CACHE t


-- === EncodeMap === --

type EncodeMap_MANUAL_CACHE t =
    'Map [ {-  9 -} '( Lit.Star                                                              , '[  9 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Lit.String                                                            , '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Lit.Number                                                            , '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons   Lit.String (Layout t Val   Static )                            , '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Lam               (Layout t Val   Static )                            , '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons              (Layout t Val   Dynamic) (Layout t Val   Dynamic)   , '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Lam               (Layout t Val   Dynamic)                            , '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Cons   Lit.String (Layout t Thunk Static )                            , '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( Acc    Lit.String (Layout t Thunk Static )                            , '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( App               (Layout t Thunk Static )                            , '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Curry             (Layout t Thunk Static )                            , '[ 19 , 3,4,5,6,7,8       ] )
         , {- 20 -} '( Lam               (Layout t Thunk Static )                            , '[ 20 , 3,4,5,6,7,8       ] )
         , {- 21 -} '( Native Lit.String                                                     , '[ 21 , 3,4,5,6,7,8       ] )
         , {- 22 -} '( Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)   , '[ 22 , 4,6,8             ] )
         , {- 23 -} '( App               (Layout t Thunk Dynamic)                            , '[ 23 , 4,6,8             ] )
         , {- 24 -} '( Curry             (Layout t Thunk Dynamic)                            , '[ 24 , 4,6,8             ] )
         , {- 25 -} '( Cons              (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)   , '[ 25 , 4,6,8             ] )
         , {- 26 -} '( Lam               (Layout t Thunk Dynamic)                            , '[ 26 , 4,6,8             ] )
         , {- 27 -} '( Native            (Layout t Thunk Dynamic)                            , '[ 27 , 4,6,8             ] )
         , {- 28 -} '( Var    Lit.String                                                     , '[ 28 , 5,6,7,8           ] )
         , {- 29 -} '( Cons   Lit.String (Layout t Phrase Static )                           , '[ 29 , 5,6,7,8           ] )
         , {- 30 -} '( Unify             (Layout t Phrase Static )                           , '[ 30 , 5,6,7,8           ] )
         , {- 31 -} '( Match             (Layout t Phrase Static )                           , '[ 31 , 5,6,7,8           ] )
         , {- 32 -} '( Acc    Lit.String (Layout t Phrase Static )                           , '[ 32 , 5,6,7,8           ] )
         , {- 33 -} '( App               (Layout t Phrase Static )                           , '[ 33 , 5,6,7,8           ] )
         , {- 34 -} '( Curry             (Layout t Phrase Static )                           , '[ 34 , 5,6,7,8           ] )
         , {- 35 -} '( Lam               (Layout t Phrase Static )                           , '[ 35 , 5,6,7,8           ] )
         , {- 36 -} '( Var               (Layout t Phrase Dynamic)                           , '[ 36 , 6,8               ] )
         , {- 37 -} '( Unify             (Layout t Phrase Dynamic)                           , '[ 37 , 6,8               ] )
         , {- 38 -} '( Match             (Layout t Phrase Dynamic)                           , '[ 38 , 6,8               ] )
         , {- 39 -} '( Acc               (Layout t Phrase Dynamic) (Layout t Phrase Dynamic) , '[ 39 , 6,8               ] )
         , {- 40 -} '( App               (Layout t Phrase Dynamic)                           , '[ 40 , 6,8               ] )
         , {- 41 -} '( Curry             (Layout t Phrase Dynamic)                           , '[ 41 , 6,8               ] )
         , {- 42 -} '( Cons              (Layout t Phrase Dynamic) (Layout t Phrase Dynamic) , '[ 42 , 6,8               ] )
         , {- 43 -} '( Lam               (Layout t Phrase Dynamic)                           , '[ 43 , 6,8               ] )
         , {- 44 -} '( Native            (Layout t Phrase Dynamic)                           , '[ 44 , 6,8               ] )
         , {- 45 -} '( Blank                                                                 , '[ 45 , 7,8               ] )
         , {- 46 -} '( Cons   Lit.String (Layout t Draft Static )                            , '[ 46 , 7,8               ] )
         , {- 47 -} '( Unify             (Layout t Draft Static )                            , '[ 47 , 7,8               ] )
         , {- 48 -} '( Match             (Layout t Draft Static )                            , '[ 48 , 7,8               ] )
         , {- 49 -} '( Acc    Lit.String (Layout t Draft Static )                            , '[ 49 , 7,8               ] )
         , {- 50 -} '( App               (Layout t Draft Static )                            , '[ 50 , 7,8               ] )
         , {- 51 -} '( Curry             (Layout t Draft Static )                            , '[ 51 , 7,8               ] )
         , {- 52 -} '( Lam               (Layout t Draft Static )                            , '[ 52 , 7,8               ] )
         , {- 53 -} '( Var               (Layout t Draft Dynamic)                            , '[ 53 , 8                 ] )
         , {- 54 -} '( Unify             (Layout t Draft Dynamic)                            , '[ 54 , 8                 ] )
         , {- 55 -} '( Match             (Layout t Draft Dynamic)                            , '[ 55 , 8                 ] )
         , {- 56 -} '( Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic)   , '[ 56 , 8                 ] )
         , {- 57 -} '( App               (Layout t Draft Dynamic)                            , '[ 57 , 8                 ] )
         , {- 58 -} '( Curry             (Layout t Draft Dynamic)                            , '[ 58 , 8                 ] )
         , {- 59 -} '( Cons              (Layout t Draft Dynamic) (Layout t Draft Dynamic)   , '[ 59 , 8                 ] )
         , {- 60 -} '( Lam               (Layout t Draft Dynamic)                            , '[ 60 , 8                 ] )
         , {- 61 -} '( Native            (Layout t Draft Dynamic)                            , '[ 61 , 8                 ] )
         ]

type instance Encode (Atom Blank' dyn a) rec = '[ 41 , 7,8               ]

-- #ifndef CachedTypeFamilies

-- FIXME
---- SubGroupRelations

--type family MapIndex els (cont :: [*]) where MapIndex '[]       cont = '[]
--                                             MapIndex (e ': es) cont = UnsafeIndex e cont ': MapIndex es cont

--type family SubGroups       g  where SubGroups       g         = (UniqueFix (SubGroups' g :: [*]) :: [*])
--type family SubGroups'      g  where SubGroups'      g         = GatherSubGroups (Groups g) <> Groups g
--type family GatherSubGroups gs where GatherSubGroups '[]       = ('[] :: [*])
--                                     GatherSubGroups (g ': gs) = SubGroups' g <> GatherSubGroups gs

--type family SubGroupRel    g  where SubGroupRel    g         = '(UnsafeIndex g (Layout_CACHE IM), MapIndex (SubGroups g :: [*]) (Layout_CACHE IM))
--type family MapSubGroupRel gs where MapSubGroupRel '[]       = ('[] :: [(Nat, [Nat])])
--                                    MapSubGroupRel (g ': gs) = SubGroupRel g ': MapSubGroupRel gs

--type SubGroupRelations_RULE = (MapSubGroupRel (GroupList IM) :: [(Nat, [Nat])])
--CACHE_AS(SubGroupRelations_RULE, SubGroupRelations)

---- SubGroupInvRelations

--type family InverseRel  arg rels where InverseRel arg rels = '(arg, InverseRel' arg rels)
--type family InverseRel' (arg :: Nat) (rels :: [(Nat, [Nat])]) where
--    InverseRel' a '[]                = '[]
--    InverseRel' a ( '(s, ts) ': rs ) = If (a `In` ts) '[s] '[] <> InverseRel' a rs

--type family MapInverseRel args rels where
--    MapInverseRel '[]       rels = '[]
--    MapInverseRel (a ': as) rels = InverseRel a rels ': MapInverseRel as rels

--type SubGroupInvRelations_RULE = (MapInverseRel (Enumerate (Size (GroupList IM))) SubGroupRelations :: [(Nat, [Nat])])
--CACHE_AS(SubGroupInvRelations_RULE, SubGroupInvRelations)

---- Relation expanders

--type        ExpandSubGroupRel  g  rels = g ': ExpandSubGroupRel' g rels
--type family ExpandSubGroupRel' g (rels :: [(Nat, [Nat])]) where
--    ExpandSubGroupRel' g '[] = '[]
--    ExpandSubGroupRel' g ( '(g, rels) ': rs ) = rels <> ExpandSubGroupRel' g rs
--    ExpandSubGroupRel' g ( r          ': rs ) =         ExpandSubGroupRel' g rs

--type family MapExpandSubGroupRel rels gs where
--    MapExpandSubGroupRel rels '[]       = '[]
--    MapExpandSubGroupRel rels (g ': gs) = ExpandSubGroupRel g rels <> MapExpandSubGroupRel rels gs

---- SubGroupInvRelations

--type family GroupsOf  v    where GroupsOf  v = UnsafeIndex v (Layout_CACHE IM) ': CatMaybes (GroupsOf' v (GroupList IM))
--type family GroupsOf' v gs where GroupsOf' v '[]       = '[]
--                                 GroupsOf' v (g ': gs) = If (v `In` X.Variants g) ('Just (UnsafeIndex g (GroupList IM))) 'Nothing ': GroupsOf' v gs

---- EncodeMapRel

--type EncodeMapRel a = UniqueFix (MapExpandSubGroupRel SubGroupInvRelations ( (GroupsOf a)))
--type family MapEncodeMapRel as where
--    MapEncodeMapRel '[] = '[]
--    MapEncodeMapRel (a ': as) = EncodeMapRel a ': MapEncodeMapRel as

---- Final rules

--type EncodeMap_RULE t = 'Map $ Zip (VariantList t) (MapEncodeMapRel (VariantList IM))
--CACHE_AS(EncodeMap_RULE, EncodeMap_GEN_CACHE)
--CHECK_EQ(EncodeMap_GEN_CACHE IM, EncodeMap_MANUAL_CACHE IM)
--type EncodeMap_CACHE t = EncodeMap_GEN_CACHE t

-- #else

type EncodeMap_CACHE t = EncodeMap_MANUAL_CACHE t

-- #endif

type instance EncodeMap (TermRecord gs vs t) = EncodeMap_CACHE t
