{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.AST.Term.Class (module Luna.Syntax.AST.Term.Class, module X) where

import           Prelude.Luna                 hiding (Num, Swapped)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (ASTRecord, Layout, Variants, Match)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Evaluation.Runtime      (Dynamic, Static, ToDynamic, ToStatic, SubRuntimes, SubSemiRuntimes, ByRuntime)
import qualified Luna.Evaluation.Runtime      as Runtime
import           Luna.Syntax.Repr.Styles
import           Luna.Syntax.AST.Arg
import qualified Data.Reprx                   as Repr
import           Type.Bool
import           Luna.Evaluation.Model
import qualified Luna.Syntax.AST.Lit          as Lit

import Data.Record as X (Data)

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



-- TODO[WD]: move to issue tracker after releasing Luna to github

--------------------------------------------
-- === Enhancement proposals & issues === --
--------------------------------------------

-- Status: pending | accepted | rejected

-- Reporter  Status   Description
-- wdanilo   pending  ACCESSORS AND FUNCTIONS UNIFICATION
--                    Check if we can throw away accessors in terms. Let's consider the following Luna code:
--                        a  = x.bar
--                        a' = acc x "bar"
--                    These lines should mean exactly the same with the followings rules:
--                        - both forms have to be distinguishable to provide Term <-> Text conversion
--                        - the performance of STATIC Luna compilation should be as fast as in current solution
--                        - accessors should be first class objects, althought we can easily make a workaround like `myacc = a : a.x`




------------------------
-- === Properties === --
------------------------

type family Name   a
type family Source a
type family Target a
type family Args   a

class HasName   a where name   :: Lens' a (Name   a)
class HasSource a where source :: Lens' a (Source a)
class HasTarget a where target :: Lens' a (Target a)
class HasArgs   a where args   :: Lens' a (Args   a)




-----------------------------
-- === Component types === --
-----------------------------


-- LEGEND
--   N   - Name
--   S   - Source
--   A/P - Args / Params

-- Layout                    N  S  A/P
newtype Var   n    = Var     n             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype Cons  n    = Cons    n             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Acc   n t  = Acc    !n !t          deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    App     t  = App       !t ![Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Unify   t  = Unify     !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Match   t  = Match     !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Lam     t  = Lam       ![Arg t] !t deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Native n t = Native !n ![t]        deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Blank      = Blank                 deriving (Show, Eq, Ord)


-- === N / T Folding === --
-- | NFunctor and TFunctor allow mapping components over the `n` and `t` param type respectively.

class NFunctor n m a a' | n m a -> a' where fmapN :: (n -> m) -> a -> a'
class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'
class MonoTFunctor t a where monoTMap :: (t -> t) -> a -> a

class NFoldable a t where foldrN :: (a -> b -> b) -> b -> t -> b
class TFoldable a t where foldrT :: (a -> b -> b) -> b -> t -> b

instance {-# OVERLAPPABLE #-}           TFoldable t Lit.Star      where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-}           TFoldable t Lit.String    where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-}           TFoldable t Lit.Number    where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-}           TFoldable t Blank         where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-}           TFoldable t (Var  n   )   where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-}           TFoldable t (Cons n   )   where foldrT _ = const ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Acc  n t')   where foldrT   = foldr ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (App    t')   where foldrT   = foldr ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Unify  t')   where foldrT   = foldr ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Match  t')   where foldrT   = foldr ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Lam    t')   where foldrT   = foldr ; {-# INLINE foldrT #-}
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Native n t') where foldrT   = foldr ; {-# INLINE foldrT #-}


-- === Instances ===

-- Bases
type instance Base Lit.Star     = Lit.Star
type instance Base Lit.String   = Lit.String
type instance Base Lit.Number   = Lit.Number

type instance Base (Lam     t)  = Proxy Lam
type instance Base (Cons  n  )  = Proxy Cons
type instance Base (Acc   n t)  = Proxy Acc
type instance Base (App     t)  = Proxy App
type instance Base (Var   n  )  = Proxy Var
type instance Base (Unify   t)  = Proxy Unify
type instance Base (Match   t)  = Proxy Match
type instance Base Blank        = Proxy Blank
type instance Base (Native n t) = Proxy Native

-- Wrappers
makeWrapped ''Var
makeWrapped ''Cons

-- Properties
type instance Name   (Var    n  ) = n
type instance Name   (Cons   n  ) = n
type instance Name   (Acc    n t) = n
type instance Name   (Native n t) = n
type instance Source (Acc    n t) = t
type instance Source (App      t) = t
type instance Source (Unify    t) = t
type instance Source (Match    t) = t
type instance Target (Lam      t) = t
type instance Target (Unify    t) = t
type instance Target (Match    t) = t
type instance Args   (App      t) = [Arg t]
type instance Args   (Lam      t) = [Arg t]
type instance Args   (Native n t) = [t]

instance HasName   (Var    n  ) where name   = wrapped'                                                  ; {-# INLINE name   #-}
instance HasName   (Cons   n  ) where name   = wrapped'                                                  ; {-# INLINE name   #-}
instance HasName   (Acc    n t) where name   = lens (\(Acc    n _) -> n) (\(Acc    _ t) n -> Acc   n t)  ; {-# INLINE name   #-}
instance HasName   (Native n t) where name   = lens (\(Native n _) -> n) (\(Native _ t) n -> Native n t) ; {-# INLINE name   #-}
instance HasSource (Acc    n t) where source = lens (\(Acc    _ s) -> s) (\(Acc    n _) s -> Acc   n s)  ; {-# INLINE source #-}
instance HasSource (App      t) where source = lens (\(App    s _) -> s) (\(App    _ a) s -> App   s a)  ; {-# INLINE source #-}
instance HasSource (Unify    t) where source = lens (\(Unify  s _) -> s) (\(Unify  _ t) s -> Unify s t)  ; {-# INLINE source #-}
instance HasSource (Match    t) where source = lens (\(Match  s _) -> s) (\(Match  _ t) s -> Match s t)  ; {-# INLINE source #-}
instance HasTarget (Lam      t) where target = lens (\(Lam    _ t) -> t) (\(Lam    s _) t -> Lam   s t)  ; {-# INLINE target #-}
instance HasTarget (Unify    t) where target = lens (\(Unify  _ t) -> t) (\(Unify  s _) t -> Unify s t)  ; {-# INLINE target #-}
instance HasTarget (Match    t) where target = lens (\(Match  _ t) -> t) (\(Match  s _) t -> Match s t)  ; {-# INLINE target #-}
instance HasArgs   (App      t) where args   = lens (\(App    _ a) -> a) (\(App    s _) a -> App   s a)  ; {-# INLINE args   #-}
instance HasArgs   (Lam      t) where args   = lens (\(Lam    a _) -> a) (\(Lam    _ o) a -> Lam   a o)  ; {-# INLINE args   #-}
instance HasArgs   (Native n t) where args   = lens (\(Native _ a) -> a) (\(Native n _) a -> Native n a) ; {-# INLINE args   #-}

-- Mapping
instance n ~ n' => NFunctor n m (Var    n'  ) (Var    m  ) where fmapN = (wrapped %~)                  ; {-# INLINE fmapN #-}
instance n ~ n' => NFunctor n m (Cons   n'  ) (Cons   m  ) where fmapN = (wrapped %~)                  ; {-# INLINE fmapN #-}
instance n ~ n' => NFunctor n m (Acc    n' t) (Acc    m t) where fmapN f (Acc n t)    = Acc (f n) t    ; {-# INLINE fmapN #-}
instance n ~ n' => NFunctor n m (Native n' t) (Native m t) where fmapN f (Native n t) = Native (f n) t ; {-# INLINE fmapN #-}
instance           NFunctor n m (Lam       t) (Lam      t) where fmapN = flip const                    ; {-# INLINE fmapN #-}
instance           NFunctor n m (App       t) (App      t) where fmapN = flip const                    ; {-# INLINE fmapN #-}
instance           NFunctor n m (Unify     t) (Unify    t) where fmapN = flip const                    ; {-# INLINE fmapN #-}
instance           NFunctor n m (Match     t) (Match    t) where fmapN = flip const                    ; {-# INLINE fmapN #-}
instance           NFunctor n m Blank         Blank        where fmapN = flip const                    ; {-# INLINE fmapN #-}

instance t ~ t' => TFunctor t r (Lam      t') (Lam      r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Acc    n t') (Acc    n r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Native n t') (Native n r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (App      t') (App      r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Unify    t') (Unify    r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Match    t') (Match    r) where fmapT = fmap       ; {-# INLINE fmapT #-}
instance           TFunctor t r (Var    n   ) (Var    n  ) where fmapT = flip const ; {-# INLINE fmapT #-}
instance           TFunctor t r (Cons   n   ) (Cons   n  ) where fmapT = flip const ; {-# INLINE fmapT #-}
instance           TFunctor t r Blank         Blank        where fmapT = flip const ; {-# INLINE fmapT #-}

instance           MonoTFunctor t Lit.Star      where monoTMap = flip const ; {-# INLINE monoTMap #-}
instance           MonoTFunctor t Lit.String    where monoTMap = flip const ; {-# INLINE monoTMap #-}
instance           MonoTFunctor t Lit.Number    where monoTMap = flip const ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (Lam      t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (Acc    n t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (Native n t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (App      t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (Unify    t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance t ~ t' => MonoTFunctor t (Match    t') where monoTMap = fmap       ; {-# INLINE monoTMap #-}
instance           MonoTFunctor t (Var    n   ) where monoTMap = flip const ; {-# INLINE monoTMap #-}
instance           MonoTFunctor t (Cons   n   ) where monoTMap = flip const ; {-# INLINE monoTMap #-}
instance           MonoTFunctor t Blank         where monoTMap = flip const ; {-# INLINE monoTMap #-}

-- Representations

-- Default
instance {-# OVERLAPPABLE #-}                   Repr s Lit.Star     where repr _                = "*"
instance {-# OVERLAPPABLE #-}                   Repr s Lit.String   where repr (Lit.String s)   = "Str"    <+> repr s
instance {-# OVERLAPPABLE #-}                   Repr s Lit.Number   where repr (Lit.Number r n) = "Num"    <+> repr r <+> repr n
instance {-# OVERLAPPABLE #-} Repr  s n      => Repr s (Var    n  ) where repr (Var        n)   = "Var"    <+> repr n
instance {-# OVERLAPPABLE #-} Repr  s n      => Repr s (Cons   n  ) where repr (Cons       n)   = "Cons"   <+> repr n
instance {-# OVERLAPPABLE #-} Repr  s t      => Repr s (Lam      t) where repr (Lam        s t) = "Lam  "  <+> repr s <+> repr t
instance {-# OVERLAPPABLE #-} Reprs s '[n,t] => Repr s (Acc    n t) where repr (Acc        n s) = "Acc"    <+> repr n <+> repr s
instance {-# OVERLAPPABLE #-} Repr  s t      => Repr s (App      t) where repr (App        s a) = "App"    <+> repr s <+> repr a
instance {-# OVERLAPPABLE #-} Repr  s t      => Repr s (Unify    t) where repr (Unify      s t) = "Unify"  <+> repr s <+> repr t
instance {-# OVERLAPPABLE #-} Repr  s t      => Repr s (Match    t) where repr (Match      s t) = "Match"    <+> repr s <+> repr t
instance {-# OVERLAPPABLE #-} Reprs s '[n,t] => Repr s (Native n t) where repr (Native     n a) = "Native" <+> repr n <+> repr a
instance {-# OVERLAPPABLE #-}                   Repr s  Blank       where repr _                = "Blank"
instance {-# OVERLAPPABLE #-}                   Repr s Lit.System   where repr                  = \case Lit.Rational r -> repr r
                                                                                                        Lit.Integer  i -> repr i
                                                                                                        Lit.Double   d -> repr d

-- HeaderOnly
instance {-# OVERLAPPABLE #-} Repr StaticNameOnly n => Repr HeaderOnly (Var   n    ) where repr (Var n) = "Var" <+> fromString (reprStyled StaticNameOnly n)
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Cons   n           ) where repr _ = "Cons"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Cons   Lit.String  ) where repr (Cons s) = fromString $ "Cons " <>  show (unwrap' s)
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Lam               t) where repr _ = "Lam"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Acc    n          t) where repr _ = "Acc"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Acc    Lit.String t) where repr (Acc s _) = fromString $ "Acc " <>  show (unwrap' s)
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (App               t) where repr _ = "App"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Unify             t) where repr _ = "Unify"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Match             t) where repr _ = "Match"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Native n          t) where repr _ = "Native"
instance {-# OVERLAPPABLE #-} Repr HeaderOnly (Native Lit.String t) where repr (Native s _) = fromString $ "Native " <> show (unwrap' s)



instance {-# OVERLAPPABLE #-} Repr StaticNameOnly Lit.String where repr = fromString ∘ show ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr StaticNameOnly a          where repr = const ""
---------------------------
---------------------------




newtype ASTRecord (groups :: [*]) (variants :: [*]) t d = ASTRecord d deriving (Show, Eq, Ord)


-- === Instances === --

type instance Props Variant (ASTRecord gs vs t d) = vs
type instance Props Group   (ASTRecord gs vs t d) = gs

type instance RecordOf (ASTRecord gs vs t d) = ASTRecord gs vs t d
instance      IsRecord (ASTRecord gs vs t d) where asRecord = id ; {-# INLINE asRecord #-}

-- Wrappers
makeWrapped ''ASTRecord
type instance Unlayered (ASTRecord gs vs t d) = Unwrapped (ASTRecord gs vs t d)
instance      Layered   (ASTRecord gs vs t d)

-- Conversions
instance Castable    (ASTRecord gs vs t d) d
instance Convertible (ASTRecord gs vs t d) d where convert = unwrap' ; {-# INLINE convert #-}
instance Castable  d (ASTRecord gs vs t d)   where cast    = wrap'   ; {-# INLINE cast    #-}





--class TermCons n t term a | a -> n t where termCons :: term -> a

--instance (t ~ t', n ~ NameByRuntime rt t, t ~ Layout tp term rt, SmartCons (Unify t') (Term tp term rt))
--      => TermCons n t (Unify t') (Term tp term rt) where termCons = cons

--class TermCons2 a cls term rt where termCons2 :: a -> Term cls term rt

--instance (t ~ t', n ~ NameByRuntime rt t, t ~ Layout cls term rt, SmartCons (Unify t') (Term cls term rt))
--      => TermCons2 (Unify t) cls term rt where termCons2 = cons


-------------------------
-- === Term groups === --
-------------------------

-- | The following definitions are parameterized by the `t` type, which indicates which data `Layout` to choose.
--   The `Layout` type family defines the recursive layout for AST structures.

newtype     Term     t term rt = Term (ASTRecord (SubRuntimeGroups rt t term) (Variants t term rt) t Data)
type        Variants t term rt = Elems term (NameByRuntime rt (Layout t term rt)) (Layout t term rt)
type family Layout   t term rt
--type family LayoutOf   a
type family LayoutType a
type family TermOf     a

type family Input     a
type family NameInput a where
    NameInput I = Impossible
    NameInput a = If (Runtime.Model a == Static) Lit.String (Input a)

--type family NameLayoutOf a where
--    NameLayoutOf I = Impossible
--    NameLayoutOf a = If (Runtime.Model a == Static) Str (LayoutOf a)



type TermGroups = '[Lit, Val, Thunk, Expr, Draft]


-- === Elems === --

type family   Elems term  n t :: [*]

type instance Elems Lit   n t = Lit.Star
                             ': Lit.String
                             ': Lit.Number
                             ': '[]

type instance Elems Val   n t = Cons        n
                             ': Lam           t
                             ': Elems Lit   n t

type instance Elems Thunk n t = Acc         n t
                             ': App           t
                             ': Native      n t
                             ': Elems Val   n t

type instance Elems Expr  n t = Var         n
                             ': Unify         t
                             ': Match         t
                             ': Elems Thunk n t

type instance Elems Draft n t = Blank
                             ': Elems Expr  n t


-- === Syntax Layouts === --

type family SubSemiTerms ts term where
    SubSemiTerms '[]       term = '[]
    SubSemiTerms (t ': ts) t    = '[t]
    SubSemiTerms (t ': ts) term = t ': SubSemiTerms ts term

type ApplySubRuntimes     rt t a = ApplyLayouts (SubRuntimes     rt) t a
type ApplySubSemiRuntimes rt t a = ApplyLayouts (SubSemiRuntimes rt) t a
type family ApplyLayouts rts t a where ApplyLayouts '[]         t a = '[]
                                       ApplyLayouts (rt ': rts) t a = Term a t rt ': ApplyLayouts rts t a

type SubRuntimeGroups rt t a = SubRuntimeGroups' rt t (SubSemiTerms TermGroups a)
type family SubRuntimeGroups' rt t gs where
  SubRuntimeGroups' rt t '[]       = '[]
  SubRuntimeGroups' rt t '[g]      = ApplySubRuntimes     rt t g
  SubRuntimeGroups' rt t (g ': gs) = ApplySubSemiRuntimes rt t g <> SubRuntimeGroups' rt t gs

type NameByRuntime rt d = ByRuntime rt Lit.String d


-- === Variant repr === --

type VariantRepr s rec = WithElement' ElemShow rec (Repr.Builder s Repr.Tok)

class                                                 ElemShow a out where elemShow :: a -> out
instance (Repr s a, Repr.Builder s Repr.Tok ~ out) => ElemShow a out where elemShow = repr

instance {-# OVERLAPPABLE #-}  VariantRepr s (ASTRecord gs vs t d)                        => Repr s          (ASTRecord gs vs t d) where repr   = variantRepr                                      ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} (VariantRepr s (Unwrapped (Term t term rt)), Typeable term) => Repr s          (Term      t term rt) where repr t = fromString (showTermType t) <+> repr (unwrap' t) ; {-# INLINE repr #-}
instance                       VariantRepr HeaderOnly (Unwrapped (Term t term rt))        => Repr HeaderOnly (Term      t term rt) where repr   = repr ∘ unwrap'                                   ; {-# INLINE repr #-}

variantRepr :: VariantRepr s rec => rec -> Repr.Builder s Repr.Tok
variantRepr = withElement' (Proxy :: Proxy ElemShow) elemShow


-- === Utils === --

showTermType :: Typeable term => Term t term rt -> String
showTermType (t :: Term t term rt) = tyConName $ typeRepTyCon $ head $ snd $ splitTyConApp $ typeOf (Proxy :: Proxy term)


-- === Instances === --

-- Basic instances
type instance Runtime.Model (Term t term rt) = rt
type instance LayoutType    (Term t term rt) = t
type instance TermOf        (Term t term rt) = Term t term rt

deriving instance Show (Unlayered (Term t term rt)) => Show (Term t term rt)
deriving instance Eq   (Unlayered (Term t term rt)) => Eq   (Term t term rt)
deriving instance Ord  (Unlayered (Term t term rt)) => Ord  (Term t term rt)

-- Bases
type instance Base (Term t term rt) = Proxy term

-- Wrappers & Layers
makeWrapped ''Term
type instance Unlayered (Term t term rt) = Unwrapped (Term t term rt)
instance      Layered   (Term t term rt)
instance      Rewrapped (Term t term rt) (Term t' term' rt')

-- Record instances
type instance RecordOf (Term t term rt) = RecordOf (Unlayered (Term t term rt))
instance IsRecord (Unlayered (Term t term rt)) => IsRecord (Term t term rt) where asRecord = wrapped' ∘ asRecord

-- Layouts
type instance ToStatic  (Term t term rt) = Term t term (ToStatic  rt)
type instance ToDynamic (Term t term rt) = Term t term (ToDynamic rt)

-- Properties
type instance Props p (Term t term rt) = Props p (RecordOf (Term t term rt))

-- Conversions
instance Unwrapped (Term t term rt) ~ ASTRecord gs vs t' d => Convertible (Term t term rt) (ASTRecord gs vs t' d) where convert = unwrap' ; {-# INLINE convert #-}

instance Convertible (Unwrapped (Term t term rt)) Data => Castable    (Term t term rt) Data
instance Convertible (Unwrapped (Term t term rt)) Data => Convertible (Term t term rt) Data where convert = convert ∘ unwrap' ; {-# INLINE convert #-}
instance Castable    Data (Unwrapped (Term t term rt)) => Castable    Data (Term t term rt) where cast    = wrap'   ∘ cast    ; {-# INLINE cast    #-}


-- Abstractions
type instance                                                       Abstract    (Term t term rt) = Data
instance BiCastable (Abstract (Term t term rt)) (Term t term rt) => IsAbstract  (Term t term rt) where abstracted = iso cast cast
instance BiCastable (Abstract (Term t term rt)) (Term t term rt) => HasAbstract (Term t term rt)



------------------------------------
-- === Term Layout type caches === --
------------------------------------

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
                                  , {-  5 -} Term t Expr  Static
                                  , {-  6 -} Term t Expr  Dynamic
                                  , {-  7 -} Term t Draft Static
                                  , {-  8 -} Term t Draft Dynamic
                                  ]
type VariantList_MANUAL_CACHE t = [ {-  9 -} Lit.Star
                                  , {- 10 -} Lit.String
                                  , {- 11 -} Lit.Number
                                  , {- 12 -} Cons   Lit.String
                                  , {- 13 -} Lam               (Layout t Val   Static )
                                  , {- 14 -} Cons              (Layout t Val   Dynamic)
                                  , {- 15 -} Lam               (Layout t Val   Dynamic)
                                  , {- 16 -} Acc    Lit.String (Layout t Thunk Static )
                                  , {- 17 -} App               (Layout t Thunk Static )
                                  , {- 18 -} Lam               (Layout t Thunk Static )
                                  , {- 19 -} Native Lit.String (Layout t Thunk Static )
                                  , {- 20 -} Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)
                                  , {- 21 -} App               (Layout t Thunk Dynamic)
                                  , {- 22 -} Cons              (Layout t Thunk Dynamic)
                                  , {- 23 -} Lam               (Layout t Thunk Dynamic)
                                  , {- 24 -} Native            (Layout t Thunk Dynamic) (Layout t Thunk Dynamic)
                                  , {- 25 -} Var    Lit.String
                                  , {- 26 -} Unify             (Layout t Expr  Static )
                                  , {- 27 -} Match             (Layout t Expr  Static )
                                  , {- 28 -} Acc    Lit.String (Layout t Expr  Static )
                                  , {- 29 -} App               (Layout t Expr  Static )
                                  , {- 30 -} Lam               (Layout t Expr  Static )
                                  , {- 31 -} Native Lit.String (Layout t Expr  Static )
                                  , {- 32 -} Var               (Layout t Expr  Dynamic)
                                  , {- 33 -} Unify             (Layout t Expr  Dynamic)
                                  , {- 34 -} Match             (Layout t Expr  Dynamic)
                                  , {- 35 -} Acc               (Layout t Expr  Dynamic) (Layout t Expr  Dynamic)
                                  , {- 36 -} App               (Layout t Expr  Dynamic)
                                  , {- 37 -} Cons              (Layout t Expr  Dynamic)
                                  , {- 38 -} Lam               (Layout t Expr  Dynamic)
                                  , {- 39 -} Native            (Layout t Expr  Dynamic) (Layout t Expr  Dynamic)
                                  , {- 40 -} Blank
                                  , {- 41 -} Unify             (Layout t Draft Static )
                                  , {- 42 -} Match             (Layout t Draft Static )
                                  , {- 43 -} Acc    Lit.String (Layout t Draft Static )
                                  , {- 44 -} App               (Layout t Draft Static )
                                  , {- 45 -} Lam               (Layout t Draft Static )
                                  , {- 46 -} Native Lit.String (Layout t Draft Static )
                                  , {- 47 -} Var               (Layout t Draft Dynamic)
                                  , {- 48 -} Unify             (Layout t Draft Dynamic)
                                  , {- 49 -} Match             (Layout t Draft Dynamic)
                                  , {- 50 -} Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic)
                                  , {- 51 -} App               (Layout t Draft Dynamic)
                                  , {- 52 -} Cons              (Layout t Draft Dynamic)
                                  , {- 53 -} Lam               (Layout t Draft Dynamic)
                                  , {- 54 -} Native            (Layout t Draft Dynamic) (Layout t Draft Dynamic)
                                  ]

#ifndef CachedTypeFamilies

FIXME
--type VariantList_RULE t = Unique (GatherProps Variant (GroupList t))
--CACHE_AS(VariantList_RULE, VariantList_GEN_CACHE)
--CHECK_EQ(VariantList_GEN_CACHE IM, VariantList_MANUAL_CACHE IM)
--type VariantList_CACHE t = VariantList_GEN_CACHE t

#else

type VariantList_CACHE t = VariantList_MANUAL_CACHE t

#endif

type VariantList t = VariantList_CACHE t

-- Layout

type Layout_RULE t = GroupList t <> VariantList t
CACHE_AS(Layout_RULE, "Layout_CACHE")

type instance Record.Layout (ASTRecord gs vs t d) = Layout_CACHE t

type instance Layout_Variants Variant (ASTRecord gs vs t d) = VariantList t

-- === DecodeMap === --

type DecodeMap_MANUAL_CACHE t =
    'Map [ {-  0 -} '( Term t Lit   Static                                                 ,  0 )
         , {-  1 -} '( Term t Val   Static                                                 ,  1 )
         , {-  2 -} '( Term t Val   Dynamic                                                ,  2 )
         , {-  3 -} '( Term t Thunk Static                                                 ,  3 )
         , {-  4 -} '( Term t Thunk Dynamic                                                ,  4 )
         , {-  5 -} '( Term t Expr  Static                                                 ,  5 )
         , {-  6 -} '( Term t Expr  Dynamic                                                ,  6 )
         , {-  7 -} '( Term t Draft Static                                                 ,  7 )
         , {-  8 -} '( Term t Draft Dynamic                                                ,  8 )
         , {-  9 -} '( Lit.Star                                                            ,  9 )
         , {- 10 -} '( Lit.String                                                          , 10 )
         , {- 11 -} '( Lit.Number                                                          , 11 )
         , {- 12 -} '( Cons   Lit.String                                                   , 12 )
         , {- 13 -} '( Lam               (Layout t Val   Static )                          , 13 )
         , {- 14 -} '( Cons              (Layout t Val   Dynamic)                          , 14 )
         , {- 15 -} '( Lam               (Layout t Val   Dynamic)                          , 15 )
         , {- 16 -} '( Acc    Lit.String (Layout t Thunk Static )                          , 16 )
         , {- 17 -} '( App               (Layout t Thunk Static )                          , 17 )
         , {- 18 -} '( Lam               (Layout t Thunk Static )                          , 18 )
         , {- 19 -} '( Native Lit.String (Layout t Thunk Static )                          , 19 )
         , {- 20 -} '( Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic) , 20 )
         , {- 21 -} '( App               (Layout t Thunk Dynamic)                          , 21 )
         , {- 22 -} '( Cons              (Layout t Thunk Dynamic)                          , 22 )
         , {- 23 -} '( Lam               (Layout t Thunk Dynamic)                          , 23 )
         , {- 24 -} '( Native            (Layout t Thunk Dynamic) (Layout t Thunk Dynamic) , 24 )
         , {- 25 -} '( Var    Lit.String                                                   , 25 )
         , {- 26 -} '( Unify             (Layout t Expr  Static )                          , 26 )
         , {- 27 -} '( Match             (Layout t Expr  Static )                          , 27 )
         , {- 28 -} '( Acc    Lit.String (Layout t Expr  Static )                          , 28 )
         , {- 29 -} '( App               (Layout t Expr  Static )                          , 29 )
         , {- 30 -} '( Lam               (Layout t Expr  Static )                          , 30 )
         , {- 31 -} '( Native Lit.String (Layout t Expr  Static )                          , 31 )
         , {- 32 -} '( Var               (Layout t Expr  Dynamic)                          , 32 )
         , {- 33 -} '( Unify             (Layout t Expr  Dynamic)                          , 33 )
         , {- 34 -} '( Match             (Layout t Expr  Dynamic)                          , 34 )
         , {- 35 -} '( Acc               (Layout t Expr  Dynamic) (Layout t Expr  Dynamic) , 35 )
         , {- 36 -} '( App               (Layout t Expr  Dynamic)                          , 36 )
         , {- 37 -} '( Cons              (Layout t Expr  Dynamic)                          , 37 )
         , {- 38 -} '( Lam               (Layout t Expr  Dynamic)                          , 38 )
         , {- 39 -} '( Native            (Layout t Expr  Dynamic) (Layout t Expr  Dynamic) , 39 )
         , {- 40 -} '( Blank                                                               , 40 )
         , {- 41 -} '( Unify             (Layout t Draft Static )                          , 41 )
         , {- 42 -} '( Match             (Layout t Draft Static )                          , 42 )
         , {- 43 -} '( Acc    Lit.String (Layout t Draft Static )                          , 43 )
         , {- 44 -} '( App               (Layout t Draft Static )                          , 44 )
         , {- 45 -} '( Lam               (Layout t Draft Static )                          , 45 )
         , {- 46 -} '( Native Lit.String (Layout t Draft Static )                          , 46 )
         , {- 47 -} '( Var               (Layout t Draft Dynamic)                          , 47 )
         , {- 48 -} '( Unify             (Layout t Draft Dynamic)                          , 48 )
         , {- 49 -} '( Match             (Layout t Draft Dynamic)                          , 49 )
         , {- 50 -} '( Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic) , 50 )
         , {- 51 -} '( App               (Layout t Draft Dynamic)                          , 51 )
         , {- 52 -} '( Cons              (Layout t Draft Dynamic)                          , 52 )
         , {- 53 -} '( Lam               (Layout t Draft Dynamic)                          , 53 )
         , {- 54 -} '( Native            (Layout t Draft Dynamic) (Layout t Draft Dynamic) , 54 )
         ]

#ifndef CachedTypeFamilies

FIXME
--type DecodeMap_RULE t = 'Map $ Zip (Layout_CACHE t) (Enumerate (Size (Layout_CACHE t)))
--CACHE_AS(DecodeMap_RULE, DecodeMap_GEN_CACHE)
--CHECK_EQ(DecodeMap_GEN_CACHE IM, DecodeMap_MANUAL_CACHE IM)
--type DecodeMap_CACHE t = DecodeMap_GEN_CACHE t

#else

type DecodeMap_CACHE t = DecodeMap_MANUAL_CACHE t

#endif

type instance DecodeMap (ASTRecord gs vs t d) = DecodeMap_CACHE t


-- === EncodeMap === --

type EncodeMap_MANUAL_CACHE t =
    'Map [ {-  9 -} '( Lit.Star                                                            , '[  9 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Lit.String                                                          , '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Lit.Number                                                          , '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons   Lit.String                                                   , '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Lam               (Layout t Val   Static )                          , '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons              (Layout t Val   Dynamic)                          , '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Lam               (Layout t Val   Dynamic)                          , '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Acc    Lit.String (Layout t Thunk Static )                          , '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( App               (Layout t Thunk Static )                          , '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( Lam               (Layout t Thunk Static )                          , '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Native Lit.String (Layout t Thunk Static )                          , '[ 19 , 3,4,5,6,7,8       ] )
         , {- 20 -} '( Acc               (Layout t Thunk Dynamic) (Layout t Thunk Dynamic) , '[ 20 , 4,6,8             ] )
         , {- 21 -} '( App               (Layout t Thunk Dynamic)                          , '[ 21 , 4,6,8             ] )
         , {- 22 -} '( Cons              (Layout t Thunk Dynamic)                          , '[ 22 , 4,6,8             ] )
         , {- 23 -} '( Lam               (Layout t Thunk Dynamic)                          , '[ 23 , 4,6,8             ] )
         , {- 24 -} '( Native            (Layout t Thunk Dynamic) (Layout t Thunk Dynamic) , '[ 24 , 4,6,8             ] )
         , {- 25 -} '( Var    Lit.String                                                   , '[ 25 , 5,6,7,8           ] )
         , {- 26 -} '( Unify             (Layout t Expr  Static )                          , '[ 26 , 5,6,7,8           ] )
         , {- 27 -} '( Match             (Layout t Expr  Static )                          , '[ 27 , 5,6,7,8           ] )
         , {- 28 -} '( Acc    Lit.String (Layout t Expr  Static )                          , '[ 28 , 5,6,7,8           ] )
         , {- 29 -} '( App               (Layout t Expr  Static )                          , '[ 29 , 5,6,7,8           ] )
         , {- 30 -} '( Lam               (Layout t Expr  Static )                          , '[ 30 , 5,6,7,8           ] )
         , {- 31 -} '( Native Lit.String (Layout t Expr  Static )                          , '[ 31 , 5,6,7,8           ] )
         , {- 32 -} '( Var               (Layout t Expr  Dynamic)                          , '[ 32 , 6,8               ] )
         , {- 33 -} '( Unify             (Layout t Expr  Dynamic)                          , '[ 33 , 6,8               ] )
         , {- 34 -} '( Match             (Layout t Expr  Dynamic)                          , '[ 34 , 6,8               ] )
         , {- 35 -} '( Acc               (Layout t Expr  Dynamic) (Layout t Expr  Dynamic) , '[ 35 , 6,8               ] )
         , {- 36 -} '( App               (Layout t Expr  Dynamic)                          , '[ 36 , 6,8               ] )
         , {- 37 -} '( Cons              (Layout t Expr  Dynamic)                          , '[ 37 , 6,8               ] )
         , {- 38 -} '( Lam               (Layout t Expr  Dynamic)                          , '[ 38 , 6,8               ] )
         , {- 39 -} '( Native            (Layout t Expr  Dynamic) (Layout t Expr  Dynamic) , '[ 39 , 6,8               ] )
         , {- 40 -} '( Blank                                                               , '[ 40 , 7,8               ] )
         , {- 41 -} '( Unify             (Layout t Draft Static )                          , '[ 41 , 7,8               ] )
         , {- 42 -} '( Match             (Layout t Draft Static )                          , '[ 42 , 7,8               ] )
         , {- 43 -} '( Acc    Lit.String (Layout t Draft Static )                          , '[ 43 , 7,8               ] )
         , {- 44 -} '( App               (Layout t Draft Static )                          , '[ 44 , 7,8               ] )
         , {- 45 -} '( Lam               (Layout t Draft Static )                          , '[ 45 , 7,8               ] )
         , {- 46 -} '( Native Lit.String (Layout t Draft Static )                          , '[ 46 , 7,8               ] )
         , {- 47 -} '( Var               (Layout t Draft Dynamic)                          , '[ 47 , 8                 ] )
         , {- 48 -} '( Unify             (Layout t Draft Dynamic)                          , '[ 48 , 8                 ] )
         , {- 49 -} '( Match             (Layout t Draft Dynamic)                          , '[ 49 , 8                 ] )
         , {- 50 -} '( Acc               (Layout t Draft Dynamic) (Layout t Draft Dynamic) , '[ 50 , 8                 ] )
         , {- 51 -} '( App               (Layout t Draft Dynamic)                          , '[ 51 , 8                 ] )
         , {- 52 -} '( Cons              (Layout t Draft Dynamic)                          , '[ 52 , 8                 ] )
         , {- 53 -} '( Lam               (Layout t Draft Dynamic)                          , '[ 53 , 8                 ] )
         , {- 54 -} '( Native            (Layout t Draft Dynamic) (Layout t Draft Dynamic) , '[ 54 , 8                 ] )
         ]

#ifndef CachedTypeFamilies

FIXME
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

#else

type EncodeMap_CACHE t = EncodeMap_MANUAL_CACHE t

#endif

type instance EncodeMap (ASTRecord gs vs t d) = EncodeMap_CACHE t




