{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic)
import Data.Graph.Builders
import Prologue            hiding (Symbol, Cons, Num, Version, cons, read, ( # ), Enum)

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Old.Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder       hiding (get)
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
import           Old.Data.Prop
import           Data.Record              hiding (Cons, Layout, cons, Value)
import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import           Data.Container.Hetero                           (Hetero(..), Any(..))
import qualified Data.Graph.Builder.Class                        as Graph
import qualified Data.Graph.Builder.Class                        as Graph.Builder
import           Data.Graph.Builder.Ref                          as Ref
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Luna.Config.Env                                 as Env
import qualified Luna.Library.Symbol                             as Symbol
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                           (Dynamics, Dynamic, Static)
import qualified Luna.Runtime.Dynamics                           as Runtime
import           Luna.Syntax.Model.Layer                         ((:<), (:<:))
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node          hiding (curry, star, star2, blank, unify)
import qualified Luna.Syntax.Model.Network.Builder.Node          as Old
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import qualified Luna.Syntax.Model.Network.Term                  as Net
-- import           Luna.Syntax.Term                               (OverBuilder, Layout_OLD, ExprRecord, overbuild, AnyExpr)
import qualified Old.Luna.Syntax.Term.Class                           as Term
import           Luna.Syntax.Term.Expr.Format                         (SuperFormats, Draft(Draft), Literal(Literal), Value(Value))
import qualified Old.Luna.Syntax.Term.Expr.Lit                            as Lit

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library

import           Luna.Syntax.Model.Network.Builder.Class ()
import qualified Luna.Syntax.Model.Network.Builder.Class as XP
import           Luna.Syntax.Model.Network.Builder.Term.Class (star2, ExprBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

import Data.Shell as Shell
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr hiding (Data)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.Syntax.Term.Expr.Class as TEST
import Luna.Syntax.Term.Expr.Class (All, cons2, Layout(..), Term, Name, Data(Data), Network2, NetworkT, consTerm, unsafeConsTerm, Term2)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

import Luna.Syntax.Model.Network.Builder.Term.Class (TermBuilder)
import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.Syntax.Term.Expr.Atom (Atoms)

import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import qualified Luna.Syntax.Term.Expr.Symbol.Named as N
import qualified Luna.Syntax.Term.Expr.Symbol.Named as Symbol
import Luna.Syntax.Term.Expr.Symbol (Sym)
import Control.Lens.Property
import Luna.Syntax.Term.Expr.Format (Format)
import TH
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import qualified Luna.Syntax.Term.Expr.Layout as Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast (Edge e) = cast e
--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast e = Edge $ cast e
-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---


prebuild :: IO (Ref Node (NetLayers :<: Net.Draft Static), NetGraph)
prebuild = runBuild2 def $ do
    Old.star
    Old.star
    Old.star

prebuild2 :: IO (Ref Node (NetLayers :<: Net.Draft Static), NetGraph)
prebuild2 = runBuild3 def $ do
    var2 "ala"

tst :: (TermBuilder Star m a, Inferable ELEMENT a m, ExprBuilder Draft m a) => m a
tst = do
    star2

-- tst' :: (Inferable ELEMENT a m, ExprBuilder Draft m a) => m (Ref Node (ls :< Expr t Draft Static))
-- tst' = do
--     star2

-- Ref' Node (ls :< Expr t Draft Static)
--
-- --------------
-- Ref (Term t Draft Static Any)
-- Ref (Link (Term t Draft Static Any))
--
-- read :: Ref a -> m a
-- write :: a -> m (Ref a)


-- var :: Name -> KnownTerm t fmt dyn Var
-- unify :: Ref (Term t fmt dyn a) -> Ref (Term t fmt dyn b) -> Ref (KnownTerm t fmt dyn Unify)

-- contains layers
{-
- nie trzeba uncoverowac i mozna sie bezposrednio ladnie na typach pattern-matchowac
- bardziej jednolita architektura - nie wymusza layerowania
- konstruktory moga byc dokladniejszych typow niz `m a`
-}




-- Term (Network ls) Draft  rt

tst2 :: (TermBuilder Star m (Ptr t tgt), PointedM t tgt g m a, MonadBuilder g m, Inferable ELEMENT (Ptr t tgt) m, ExprBuilder Draft m (Ptr t tgt)) => m a
tst2 = do
    s <- tst
    read s

-- read :: a -> m (Expr a)
--
-- read :: Mark Draft Static a -> m (ls :< Expr Draft Static a)
--
-- Ref Node (ls :<: Net.Draft Static) --> read
--          (ls :<: Net.Draft Static)
--
--
-- mkterm :: m a
-- -- a moze byc np. Pointerem
--
-- Mark Draft Static (Ref Node (NetLayers :<: Net.Draft Static)) --> read
--                             (NetLayers :<: Net.Draft Static)
--
--
-- Mark Draft Static (NetLayers :< Loc Node) --> read




runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             $ runNetworkBuilderT g m


runBuild2 (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             $ runNetworkBuilderT2 g m


runBuild3 (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             . runNetworkBuilderT2 g
                             . XP.runNetworkBuilderT
                             $ m

evalBuild = fmap snd ∘∘ runBuild
--TODO: test1
-- TODO: test2




------------------
-- === Refs === --
------------------

type family Referred a
newtype Ref2 a = Ref2 (Referred a)
makeWrapped ''Ref2

class Monad m => Referable m a where
    refer'  :: a -> m (Referred a)
    read2'  :: Referred a -> m a
    write2' :: Referred a -> a -> m ()

refer :: Referable m a => a -> m (Ref2 a)
refer = Ref2 <∘> refer' ; {-# INLINE refer #-}

read2 :: Referable m a => Ref2 a -> m a
read2 = read2' ∘ unwrap' ; {-# INLINE read2 #-}

write2 :: Referable m a => Ref2 a -> a -> m ()
write2 = write2' ∘ unwrap' ; {-# INLINE write2 #-}

-- === Instances === --

deriving instance Show (Referred a) => Show (Ref2 a)


------------------
-- === Binds === --
------------------

type family Binded a
newtype Bind a = Bind (Binded a)
makeWrapped ''Bind

class Monad m => Bindable m a where
    bind' :: a -> m (Binded a)

bind :: Bindable m a => a -> m (Bind a)
bind = Bind <∘> bind' ; {-# INLINE bind #-}



------------------------------------
-- === Example implementation === --
------------------------------------


----------------------
-- === IntLayer === --
----------------------

-- === Definitions === --

data IntLayer = IntLayer deriving (Show)

type instance LayerData (NetLayer t IntLayer) = Int

-- === Instances === --

instance Monad m => Creator m (Layer (NetLayer t IntLayer)) where create = return $ Layer 7


---------------------
-- === Network === --
---------------------

-- == Definitions === --

data Net (ls :: [*])
newtype NetLayer t a = NetLayer a deriving (Show, Functor, Traversable, Foldable)
newtype NetRef     a = NetRef   a deriving (Show, Functor, Traversable, Foldable)


-- === Instances === --

-- Type relations
-- type instance LayerData (NetLayer t a) = LayerData a

-- Layout_OLD
-- type TermShell ls term rec = (NetLayer term <$> ls) :| rec
-- type instance Layout_OLD (Net ls) fmt dyn sel = TermShell ls (Expr (Net ls) fmt dyn sel) (ExprRecord (Net ls) fmt dyn sel Int) -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)
-- type instance Layout_OLD (Net ls) fmt dyn sel = ExprRecord (Net ls) fmt dyn sel Int -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)

-- Shell
-- type instance Shell.Access l (Expr (Net ls) fmt dyn sel) = NetLayer (Expr (Net ls) fmt dyn sel) l

-- Ref
-- type instance Referred (Expr (Net ls) fmt dyn sel) = NetRef (Expr (Net ls) fmt dyn sel)
-- instance Monad m => Referable m (Expr (Net ls) fmt dyn sel) where refer' = return ∘ NetRef



----------------------------------



    -- type TRex2 t fmt dyn sel = ExprRecord t fmt dyn sel Int -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)


--
--
-- blank  = () ^. from symbolArgs :: Symbol Blank Static Int
-- -- -- t1 = Record.cons blank :: AnyExpr SNet Draft Static
-- t1 = Record.cons blank :: TRex2 t Draft Static 'Nothing
--
-- t2 = runIdentity $ overbuild t1 :: AnyExpr (Net '[IntLayer]) Draft Static
--
-- -- t2' = runIdentity $ buildElem $ blank :: AnyExpr (Net '[IntLayer]) Draft Static
--
-- t3 = runIdentity $ refer t2

-- t4 :: OverBuilder Identity (Expr t fmt dyn sel) => a -> Ref2 (Expr t fmt dyn sel)
-- t4 a = runIdentity $ foox a
--
--
-- type Foox = Record.Variants (RecordOf (AnyExpr SNet Draft Static))


    -- instance (Creator m c, OverBuilder m a) => OverBuilder m (Cover c a) where
    --     overbuild a = Cover <$> create <*> overbuild a
    --
    -- type ElemBuilder atom m a = (Record.Cons atom (RecordOf a), OverBuilder m a)
    -- type RefBuilder  atom m a = (ElemBuilder atom m a, Referable m a)
    --
    -- buildElem :: ElemBuilder atom m a => atom -> m a
    -- buildElem = overbuild ∘ Record.cons ; {-# INLINE buildElem #-}
    --
    -- buildRef :: RefBuilder atom m a => atom -> m (Ref2 a)
    -- buildRef = buildElem >=> refer ; {-# INLINE buildRef #-}

-- powinnismy wprowadzic abstrakcje Bind, ktora moglaby miec source i target i w networku reprezentowalaby connection

--DataConI Luna.Syntax.Term.Expr.Symbol.Unify (ForallT [KindedTV dyn_1761715480 StarT,KindedTV a_1761715481 StarT] [] (AppT (AppT ArrowT (VarT a_1761715481)) (AppT (AppT ArrowT (VarT a_1761715481)) (AppT (AppT (AppT (ConT Luna.Syntax.Term.Expr.Symbol) (ConT Luna.Syntax.Term.Expr.Atom.Unify)) (VarT dyn_1761715480)) (VarT a_1761715481))))) Luna.Syntax.Term.Expr.Symbol

-- class Monad m => OverBuilder m a where
--     overbuild :: RecordOf a -> m a

-- tx = Record.cons Atom.Blank :: Term Int Draft Runtime.Dynamic


-- Term Network2 '[Int] Static Draft Static Draft
-- Expr3 Network2 '[] '[Int] Static Draft Static App
--
-- Expr3 Network2 '[] '[Int] (Layout Static Draft) (Layout Static App)


data ZZ = AA | BB


-- #define CASE $(testTH [| case
-- #define ESAC {--}|])

runCase :: Term t model -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    s   = unwrap' $ get @Sym $ unwrap' $ get @Data el
    idx = unwrap' $ get @Atom $ unwrap' $ get @Data el
{-# INLINE runCase #-}


matchx f = f . unsafeCoerce
{-# INLINE matchx #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}

--
    -- type A1 = Term Network2 '[] '[Int] (Layout Dynamic Draft) (Layout Static Unify)
    -- type A2 = Term Network2 '[] '[Int] (Layout Static Value) (Layout Dynamic Draft)
    --
    -- type NTerm  dict fields dyn scope dyn' scope' = Term Network2 dict fields (Layout dyn scope) (Layout dyn' scope')
    -- type NTerm' dict fields dyn scope             = NTerm dict fields dyn scope dyn scope
    -- type Expr  dyn scope dyn' scope' = Term Network2 '[] '[Int] (Layout dyn scope) (Layout dyn' scope')
    -- type Expr' dyn scope             = Expr dyn scope dyn scope

-- Ref Node (Expr Static Value)


-- instance (Monad m, TEST.Cons2 a m t, Constructor' m (Ref Edge t))
--       => TEST.Cons2 a m (Ref Edge t) where
--     cons2 = construct' <=< cons2

instance (Monad m, TEST.Cons2 a m t) -- , Constructor' m (Ref Edge t))
      => TEST.Cons2 a m (Ref Edge t) where
    cons2 _ = return $ Ptr 0 -- construct' <=< cons2

-- type instance TEST.Bound Network2 dict = Ref Edge (Expr' (Get Dynamics (Get TEST.SubModel dict))
--                                                          (Get Format   (Get TEST.SubModel dict))
--                                                   )





    -- test_g1 :: forall m . PrimMonad m
    --         => m (Ref Edge (Expr' Static Value), Hetero (NEC.MGraph (PrimState m) Any Any Any))
    -- test_g1 = do
    --     v <- Hetero <$> NEC.unsafeThaw def
    --     flip Graph.Builder.runT v $ cons2 N.star
    --
    -- test_g2 :: forall m . PrimMonad m
    --         => m (Ref Edge (Expr' Static Value), Network3 m)
    -- test_g2 = do
    --     g <- NEC.emptyHMGraph
    --     flip Graph.Builder.runT g $ cons2 N.star



    -- starx :: forall m dict fields dyn scope dyn' scope' g .
    --          (TEST.Cons2 (Symbol Star dyn' (Ref Edge (Expr' dyn scope))) m (Ref Edge (NTerm dict fields dyn scope dyn' scope')))
    --       => m (Ref Edge (NTerm dict fields dyn scope dyn' scope'))
    -- starx = cons2 (star :: Symbol Star dyn' (Ref Edge (Expr' dyn scope)))
    --
    --
    -- starx2 :: forall m dict fields dyn scope dyn' scope' g .
    --           (TEST.Cons2 (Symbol Star Static (Ref Edge (NTerm' dict fields Static Value))) m (Ref Edge (NTerm' dict fields Static Value)))
    --        => m (Ref Edge (NTerm' dict fields Static Value))
    -- starx2 = cons2 (star :: Symbol Star Static (Ref Edge (NTerm' dict fields Static Value)))

    -- type MyTerm  dict layers dyn scope dyn' scope' = Term  Network2 dict layers (Layout dyn scope) (Layout dyn' scope')
    -- type MyTerm' dict layers dyn scope             = Term' Network2 dict layers (Layout dyn scope)

-- class TermCons a m where
--     -- termCons :: Symbol a dyn' (Ref Edge (Term' X dict fields model)) -> m (Ref Edge (Term X dict fields model dyn' scope'))
--     -- termCons :: Symbol a dyn' (Ref Edge (Term' X dict fields (Layout dyn scope))) -> m (Ref Edge (Term X dict fields (Layout dyn scope) (Layout dyn' (a ^. Format))))
--     -- termCons :: Symbol a dyn' (Ref Edge (MyTerm' dict fields dyn scope)) -> m (Ref Edge (MyTerm dict fields dyn scope dyn' (a ^. Format)))
--     termCons :: Symbol a layout -> m (Ref Edge (MyTerm dict fields sublayout (XX layout)))
--
--     -- termCons :: Symbol a dyn  (Ref Edge (MyTerm' dict fields model)) -> m (Ref Edge (MyTerm dict fields model (SymModel a dyn)))

-- -- test_desc1 :: TEST.Cons2 (Symbol Star Static Int) m (Ref Edge (NTerm dict fields dyn scope dyn' scope')) => m (Ref Edge (NTerm dict fields dyn scope dyn' scope'))
-- test_desc1 :: forall m dict fields dyn scope dyn' scope' g .
--               (TEST.Cons2 (Symbol Star dyn' (Ref Edge (Expr' dyn scope))) m (Ref Edge (NTerm dict fields dyn scope dyn' scope')))
-- -- dojsc do tego ->            --   TermCons Star m (Expr' Static Draft)
-- -- i zastanowic sie jaka dac abstrakcje nad refami by mogly zostac zawsze niezaleznie od grafu lub nie
--            => m (Ref Edge (NTerm dict fields dyn scope dyn' scope'))
    -- test_desc1 = do
    --     s1 <- starx2
    --     s2 <- starx2
    --     return s1



-- auto_star ::
--
-- NamedSymbol Star String Draft
--
-- Symbol t Star (Named String Draft)
--
-- Expr t (Named String Draft)
--
-- Expr t (Named String (Unify :-> Draft))
--
-- Expr t (Named (Draft ) (Unify :@ Draft))
--
-- Expr t (Named String Draft)

-- Terms Draft ... -- < = >
-- DraftCons Network2 Unify m
--
-- TermCons Network2 Unify m Draft
-- po co ten Draft ...


    -- class TermCons atom m t where
    --     termCons :: Symbol atom (ModelLayout t model) -> m (Ref Edge (Term t model))
    --
    --
    -- class TermCons m where
    --     termCons :: forall atom layout t model. TEST.Cons2 (Symbol atom layout) m (Ref Edge (Term t model))
    --              => Symbol atom layout -> m (Ref Edge (Term t model))
-- --
-- instance TermCons m where
--     termCons = cons2

    -- foos :: TermCons m => Symbol atom layout -> m (Ref Edge (Term t model))
    -- foos = termCons

-- test :: TermCons Draft t model

-- blank_x :: forall n a m t model .
--            (TEST.Cons2 (Symbol Blank (Layout.Named n a)) m (Term t model), _)
--         => m (Term t model)
-- blank_x = cons2 (N.blank :: Symbol Blank (Layout.Named n a))



-- unify_x :: (t~Network2, xmodel1 ~ xmodel2, xmodel1 ~ Layout.Named Draft Draft, Monad m
--            , model ~ MatchModels xmodel1 xmodel2)
--         => Ref Edge (Term t xmodel1) -> Ref Edge (Term t xmodel2) -> m (Ref Edge (Term t model))
-- unify_x l r = cons2 $ N.unify l r


-- type NTerm n a = Term (Named n a)
--
-- star :: NTerm () ()
--
-- unfiy star star :: NTerm () Phrase
--

-- test_sig1 :: TermCons m t
-- test_sig1 = do
--     s1 <- star
--     u1 <- unify s1 s1
--     return u1



type instance TEST.Bind t Network2 model = Ref Edge (Term Network2 (TEST.Subscope t model))
-- type instance TEST.Bind Name Network2 model = Ref Edge (Term Network2 (TEST.Subscope Name model))

type instance TEST.Subscope Atom (Layout.Named n t) = Layout.Named n (TEST.Subscope Atom t)
type instance TEST.Subscope Atom Draft = Draft
type instance TEST.Scope    Atom Draft = Draft

type instance TEST.Subscope Name (Layout.Named n t) = Layout.Named n n

type Network3 m = NEC.HMGraph (PrimState m) '[Node, Edge, Cluster]




type instance TEST.Bind2 t Network2 model = Ref Edge (Term2 Network2 (TEST.Subscope t model))
type instance TEST.Bind2 t (NetworkT n) model = Ref Edge (Term2 (NetworkT n) (TEST.Subscope t model))

type instance TEST.Subscope Atom (Layout.TNA t n a) = Layout.TNA t n (TEST.Subscope Atom a)
type instance TEST.Subscope Name (Layout.TNA t n a) = Layout.TNA t n n
type instance TEST.Scope    Atom (Layout.TNA t n a) = TEST.Scope Atom a

type instance TEST.Subscope Atom (LayoutX SimpleX '[Atom := Draft]) = LayoutX SimpleX '[Atom := Draft]



type instance TEST.BindModel Sym s (Layout.TNA t n c) = Layout.Named (TEST.Bind2 Name s (Layout.TNA t n c))
                                                                     (TEST.Bind2 Atom s (Layout.TNA t n c))


type instance TEST.BindModel Sym s (LayoutX l bs) = Layout.Named (TEST.Bind2 Name s (LayoutX l bs))
                                                                 (TEST.Bind2 Atom s (LayoutX l bs))


type instance TEST.BindModel Sym s (LayoutY l ks vs) = Layout.Named (TEST.Bind2 Name s (LayoutY l ks vs))
                                                                    (TEST.Bind2 Atom s (LayoutY l ks vs))


type instance TEST.Scope    t (LayoutX l bs) = TEST.Scope t (bs ^. t)

type instance TEST.Scope    t (LayoutY l ks vs) = TEST.Scope t ((LayoutY l ks vs) ^. t)




data G = G
type instance TEST.Fields G = '[]

xstar :: (Monad m, TEST.ASTBuilder (NetworkT a) m) => m (Term2 (NetworkT a) (Layout.TNA Draft () Draft))
xstar = consTerm N.star

xstar2 :: (Monad m, TEST.ASTBuilder (NetworkT a) m) => m (Term2 (NetworkT a) (LayoutX SimpleX '[Atom := Draft]))
xstar2 = consTerm N.star

xstar3 :: (Monad m, TEST.ASTBuilder (NetworkT a) m) => m (Term2 (NetworkT a) (Set Atom Star (DefaultLayout l ks)))
xstar3 = unsafeConsTerm N.star

xstar4 :: TEST.ASTBuilder (NetworkT a) m => m (Term2 (NetworkT a) (TNA SimpleX Draft Draft Star))
xstar4 = xstar3

-- xstar2 :: (Monad m, TEST.ASTBuilder (NetworkT a) m) => m (Term2 (NetworkT a) l)
-- xstar2 = consTerm N.star

type DefaultLayout l ks = LayoutY l ks (DefaultScopes l ks)


type family DefaultScopes l ks where
    DefaultScopes l '[]       = '[]
    DefaultScopes l (k ': ks) = DefaultScope l k ': DefaultScopes l ks

type family DefaultScope l k

type instance DefaultScope SimpleX k = Draft

type family LookupAssoc k s where
    LookupAssoc k '[]            = 'Nothing
    LookupAssoc k (k ':= v ': _) = 'Just v
    LookupAssoc k (l ':= _ ': s) = LookupAssoc k s

type instance Set k v (LayoutY l ks vs) = LayoutY l ks (SetByKey k v ks vs)
type instance Get k   (LayoutY l ks vs) = GetByKey k ks vs

type family SetByKey k v ks vs where
    SetByKey k v (k ': _)  (_ ': vs) = v ': vs
    SetByKey k n (_ ': ks) (v ': vs) = v ': SetByKey k n ks vs

type family GetByKey k ks vs where
    GetByKey k (k ': _)  (v ': _)  = v
    GetByKey k (_ ': ks) (_ ': vs) = GetByKey k ks vs


type family MatchModels m1 m2
type family UniScope t1 t2

type instance MatchModels (Layout.Named n1 t1) (Layout.Named n2 t2) = Layout.Named (UniScope n1 n2) (UniScope t1 t2)

type instance MatchModels (LayoutX t bs) (LayoutX t bs') = LayoutX t (MatchByKeys (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs)))) bs bs')

type family MatchByKeys (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: [Assoc * *] where
    MatchByKeys '[] bs bs' = '[]
    MatchByKeys (k ': ks) bs bs' = (k ':= MatchFinal (LookupAssoc k bs) (LookupAssoc k bs')) ': MatchByKeys ks bs bs'

type family MatchFinal l r where
    MatchFinal 'Nothing  ('Just a)  = a
    MatchFinal ('Just a) 'Nothing   = a
    MatchFinal ('Just a) ('Just a') = MatchXScopes a a'

type family MatchXScopes a b
type instance MatchXScopes Draft Draft = Draft
type instance MatchXScopes Draft Value = Draft


type instance MatchModels (Layout.TNA t n a) (Layout.TNA t' n' a') = Layout.TNA (UniScope t t') (UniScope n n') (UniScope a a')


type instance UniScope Draft Draft = Draft


xunify :: forall t a m model model1 model2 layout n x.
          ( t ~ NetworkT a, Monad m, model ~ MatchModels model1 model2, TEST.ASTBuilder t m
          , TEST.MatchModel (Symbol Unify layout) t model, layout ~ Layout.Named n x)
        => Ref Edge (Term2 t model1) -> Ref Edge (Term2 t model2) -> m (Term2 t model)
xunify l r = consTerm $ N.unify (unsafeCoerce l) (unsafeCoerce r)

-- moze zakodowac glebiej zaleznosci - w Symbolach ?
-- moznaby pisac wtedy np.
-- data    instance Symbol Acc      layout = Acc     !(Bind Name layout) !(Bind Child layout)
-- lub cos podobnego, przycyzm layout musialby zawierac `sys` !

data SimpleX

data LayoutX t (ls :: [Assoc * *])
data LayoutY t (keys :: [*]) (scopes :: [*])
data LayoutZ l (scopes :: [*])

type family Scopes a :: [*]
data Z_TNA
type instance Scopes Z_TNA = '[Type, Name, Atom]'

type TNA l t n a = LayoutY l '[Type, Name, Atom] '[t, n, a]

main :: IO ()
main = do
    -- print blank
    print N.blank
    -- print $ (runIdentity (encodeStore blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    print $ (runIdentity (encodeStore N.blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    -- let e1  = (runIdentity (cons2 blank  ) :: Term Network2 '[] '[Int] (Layout Static Draft) (Layout Static Draft))
    -- let e1   = (runIdentity (cons2 N.blank) :: Term Network2 '[] '[Int] (Layout N.String Draft) (Layout N.String Draft))
    -- let e1'  = (runIdentity (cons2 N.blank) :: Term2 Network2 '[] '[Int] (Layout.Named N.String Draft))
    let e1  = (runIdentity (cons2 N.blank) :: Term Network2 (Layout.Named N.String Draft))
        er1 = (runIdentity (cons2 N.blank) :: Ref Edge (Term Network2 (Layout.Named Draft Draft)))
        -- u1  = (runIdentity (unify_x er1 er1) :: Ref Edge (Term Network2 (Layout.Named Draft Draft)))

        xe1 = (runIdentity (consTerm N.blank) :: (Term2 Network2 (Layout.TNA Draft Draft Draft)))

        fs1 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Draft))
        fs2 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Draft))
        -- fu1 = runIdentity $ xunify fs1 fs2 :: Int


    -- let e2  = (runIdentity (cons2 blank  ) :: Expr' Static Draft)
    -- let e2 = (runIdentity (cons2 N.blank) :: Expr' Static Draft)
    -- let es1 = (runIdentity (cons2 star) :: Ref Edge (Expr' Static Value))
    -- let eb1 = (runIdentity (cons2 blank) :: Ref Edge (Expr' Static Draft))
    -- let eu1 = (runIdentity (cons2 $ unify eb1 eb1) :: Expr' Static Draft)
    -- let eu2 = (runIdentity (cons2 $ unify es1 es1) :: Expr Static Value Static Draft)
    -- print e2

    putStrLn ""
    print e1
    print $ get @Data e1
    print $ get @Sym $ unwrap' $ get @Data e1
    print $ unwrap' $ get @Atom $ unwrap' $ get @Data e1
    --
    -- (a,g) <- test_g2
    -- print a

    -- print $ unwrap' $ get @Atom $ unwrap' $ get @Symbol e1
    -- let a = AA
    -- case a of
    --     BB -> print "tsr"

    -- case3 (view (List.access' ExprData) e1) $
    --     of3 $ \(Symbol.Unify l r) -> (print "hello" :: IO ())
            --
            -- case3 (get @Symbol e1) $ do
            --     of3 $ \(Symbol.Unify l r) -> print "hello"
            --     of3 $ \(Symbol.Blank)     -> print "hello2"

    -- CASE e1 of
    --     Symbol.Unify l r -> print "hello"
    --     Symbol.Blank     -> print "hello2"
    -- ESAC
    --



    -- $(testTH2 'e1 ( \(Symbol.Unify l r) -> print "hello"  :: IO ()
    --               , \Symbol.Blank       -> print "hello2" :: IO ()
    --               )
    --  )


    let xx = (let blank = 2 in blank)

    --
    let { exp = e1
    ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
        ala = 11
    ;f2 = matchx $ \Symbol.Blank       -> (print "hello2" )
       where ala = 11
    } in $(testTH2 'exp [ [p|Symbol.Unify l r|], [p|Symbol.Blank|] ] ['f1, 'f2])

    -- let { exp = e1
    -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
    --     ala = 11
    -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])

    case' e1 of
        Symbol.Unify l r -> print 11
        Symbol.Blank     -> case' e1 of
            Symbol.Unify l r -> print "hello"
            Symbol.Blank     -> print "hello3x"


    -- $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])


    --
    -- case' e1 of
    --     Symbol.Unify l r -> print "hello"
    --     Symbol.Blank     -> print "hello2"


    -- runCase e1 [
    --     matchx $ \(Symbol.Unify l r) -> print "hello"
    --     matchx $ \Symbol.Blank       -> print "hello2"
    --
    -- ]

    -- runCase e1 $ [ \_ -> print "1"
    --                                             , \_ -> print "2"
    --                                             ]

    return ()

    -- print $ view (List.access' ExprData) e1 -- Refactor, List is in Fact TMap



    -- case' e1 $
    --     of'                   $ \(Symbol.Unify l r) -> print "hello"
    --     match  Unify          $ \unify              -> print "hello2"
    --     match  (Unify || App) $ \ua                 -> print "hello3"
    --     static                $ \s                  -> print "static"
    --     :_                    $                        print "oh!"


    -- E.main

    -- TEST.main
    -- (_,  g :: NetGraph ) <- prebuild2
    -- -- renderAndOpen [ ("xg", "xg", g)
    -- --               ]
    -- main2
    -- main3
        --
        -- main2 = do
        --
        --     print t2
        --     -- print $ t2 ^. Shell.access' IntLayer
        --
        --     caseTest t2 $ do
        --         of' $ \Blank -> print "it is Blank!"
        --
        -- main3 :: IO ()
        -- main3 = do
        --     (n,g) <- flip GraphBuilder.runT (def :: NetGraph2) $ do
        --         (n1 :: Ref2 (AnyExpr (Net '[IntLayer]) Draft Static)) <- buildRef blank
        --         return n1
        --     print n
        --
        --     return ()


-- refactor to Data.Construction ?
