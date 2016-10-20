{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic, Connection)
import Data.Graph.Builders hiding (Linkable)
import Prologue            hiding (Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter)

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Old.Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder       hiding (get, Linkable)
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
-- import           Old.Data.Prop
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
-- import           Luna.Syntax.Model.Network.Builder.Node          hiding (curry, star, star2, blank, unify)
import qualified Luna.Syntax.Model.Network.Builder.Node          as Old
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import qualified Luna.Syntax.Model.Network.Term                  as Net
-- import           Luna.Syntax.Term                               (OverBuilder, Layout_OLD, ExprRecord, overbuild, AnyExpr)
import qualified Old.Luna.Syntax.Term.Class                           as Term
import           Luna.Syntax.Term.Expr.Format

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library

import           Luna.Syntax.Model.Network.Builder.Class ()
import qualified Luna.Syntax.Model.Network.Builder.Class as XP
-- import           Luna.Syntax.Model.Network.Builder.Term.Class (star2, ExprBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

-- import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr hiding (Data, cons)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.Syntax.Term.Expr.Class as TEST
import Luna.Syntax.Term.Expr.Class hiding (Bind, Fields, (:=)) -- (Model, Name, All, cons2, Layout(..), Term, Term3, Data(Data), Network2, NetworkT, consTerm, unsafeConsTerm, term, Term2)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

import Luna.Syntax.Model.Network.Builder.Term.Class (ExprBuilder)
import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.Syntax.Term.Expr.Atom (Atoms)

import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import qualified Luna.Syntax.Term.Expr.Symbol.Named as N
import qualified Luna.Syntax.Term.Expr.Symbol.Named as Symbol
import qualified Luna.Syntax.Term.Expr.Symbol2 as S2
import Luna.Syntax.Term.Expr.Symbol (Sym)
import Control.Lens.Property hiding (Constructor)
import Luna.Syntax.Term.Expr.Format (Format, Sub)
import TH
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import Luna.Syntax.Term.Expr.Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)
import qualified Type.List as TList
import qualified Control.Monad.State as State
import Control.Monad.State hiding (get, set)

import System.Exit (exitSuccess)
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import qualified Luna.Syntax.Model.Network.Builder.Type as Type

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers



runCase :: Getter Data (Expr2 t layers layout) => Expr2 t layers layout -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    s   = unwrap' $ get @Sym $ unwrap' $ get @Data el
    idx = unwrap' $ get @Atom $ unwrap' $ get @Data el
{-# INLINE runCase #-}


matchx f = f . unsafeCoerce
{-# INLINE matchx #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}

--



type Network3 m = NEC.HMGraph (PrimState m) '[Node, Edge, Cluster]









type family UniScope t1 t2

type instance MatchLayouts (Layout.Named n1 t1) (Layout.Named n2 t2) = Layout.Named (UniScope n1 n2) (UniScope t1 t2)




type family MatchXScopes a b
type instance MatchXScopes Draft Draft = Draft
type instance MatchXScopes Draft Value = Draft


-- type instance MatchLayouts (Layout.TNA t n a) (Layout.TNA t' n' a') = Layout.TNA (UniScope t t') (UniScope n n') (UniScope a a')


type instance UniScope Draft Draft = Draft
type instance UniScope Value Draft = Draft



-- moze zakodowac glebiej zaleznosci - w Symbolach ?
-- moznaby pisac wtedy np.
-- data    instance Symbol Acc      layout = Acc     !(Bind Name layout) !(Bind Child layout)
-- lub cos podobnego, przycyzm layout musialby zawierac `sys` !

data SimpleX












type ANTLayout l a n t = Compound l '[Atom := a, Name := n, Type := t]




data Net = Net


type instance Get p   (Ref2 t a) = Get p a
type instance Set p v (Ref2 t a) = Ref2 t (Set p v a)





type instance Sub Atom (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Atom a) n t
type instance Sub Name (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Type t) (Sub Type t) (Sub Type t)





type instance MatchLayouts (Form a) (Form a) = Form a
-- type instance MatchLayouts Value Value = Value
-- type instance MatchLayouts Draft Draft = Draft





type family ExtendModel c l t
-- type instance ExtendModel c (ExprX layers a n t) (ExprX layers a' n' t') = Set Model (ExtendModel c (ExprX layers a n t ^. Model) (ExprX layers a' n' t' ^. Model)) (ExprX layers a n t)

type instance ExtendModel Atom (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX (Merge a a') (Merge n n') (Merge t t')
type instance ExtendModel Name (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX a (Merges '[n, a', n', t']) t
type instance ExtendModel Type (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX a n (Merges '[t, a', n', t'])

type family Merges lst where
    Merges '[a]      = a
    Merges (a ': as) = Merge a (Merges as)

type family Specialized t spec layout

-- type instance Specialized p spec (ExprX layers a n t) = Set Model (Specialized p spec (ExprX layers a n t ^. Model)) (ExprX layers a n t)
type instance Specialized Atom spec (ANTLayout l a n t) = ANTLayout l (Simplify (spec :> a)) n t





type ExprInferable t (layers :: [*]) m = (Inferable2 InfLayers layers m, Inferable2 TermType t m)
type ExprBuilder2 t layers m = (LayersCons2' t layers m, Bindable t m, Dispatcher2 Node m (Binding (AnyExpr t layers)))
type ExprBuilder_i t layers m = (ExprBuilder2 t layers m, ExprInferable t layers m)



magicStar :: (LayersCons2' t layers m, Bindable t m) => m $ Binding (Expr t layers layout)
magicStar = mkBinding =<< expr (wrap' N.star')
-- FIXME: wrap' is pure magic - it just doesnt construct ExprSymbol the right way, so we
--        can omit checking if the layout is valid


star1 :: (LayersCons2' t layers m, ValidateLayout layout Atom Star) => m (Expr t layers layout)
star1 = expr N.star'

star2 :: LayersCons2' t layers m => m (PrimExpr' t layers Star)
star2 = star1

star3 :: ExprBuilder2 t layers m => m $ Binding (PrimExpr' t layers Star)
star3 = universalDispatch Node =<< mkBinding =<< star2

-- star3' :: ExprBuilder2 t layers m => m $ Binding (PrimExpr' t layers Star)
-- star3' = universalDispatch Node =<< universalBind3' =<< star2

star4 :: ExprBuilder_i t layers m => m $ Binding (PrimExpr' t layers Star)
star4 = star3 -- inferTerm star3

-- inferTerm :: ExprInferable (a ^. TermType) (a ^. Layers) m => m a -> m a
-- inferTerm = id ; {-# INLINE inferTerm #-}


-- type AnyExpr ls = Term3 (ExprX2 ls (Uniform Draft))

xunify :: (layout ~ MatchLayouts l1 l2, LayersCons2' t ls m, ValidateLayout layout Atom Unify)
        => Binding (Expr t ls l1) -> Binding (Expr t ls l2) -> m (Expr t ls layout)
xunify l r = expr $ N.unify' (unsafeCoerce l) (unsafeCoerce r)


xunify2 :: (MonadFix m, layout ~ MatchLayouts l1 l2, LayersCons2' t ls m, ValidateLayout layout Atom Unify, Bindable t m, Linkable' t m)
        => Binding (Expr t ls l1) -> Binding (Expr t ls l2) -> m (Binding (Expr t ls layout))
xunify2 a b = mdo
    n  <- mkBinding =<< (expr $ N.unify' la lb)
    la <- mkLink n (unsafeCoerce a)
    lb <- mkLink n (unsafeCoerce b)
    return n

-- zrobic connectiony!
                -- xunify :: forall t a m layout layout1 layout2 layout n x.
                --           ( t ~ NetworkT a, Monad m, layout ~ MatchLayouts layout1 layout2, TEST.ASTBuilder t m
                --           , TEST.MatchLayout (Symbol Unify layout) t layout, layout ~ Layout.Named n x)
                --         => Ref Edge (Term2 t layout1) -> Ref Edge (Term2 t layout2) -> m (Term2 t layout)
                -- xunify l r = consTerm $ N.unify (unsafeCoerce l) (unsafeCoerce r)



universalDispatch t a = a <$ dispatch t (universal a)



--
-- type family DefaultLayout defAtom t :: Constraint
-- -- type instance DefaultLayout defAtom (ExprX layers a n t) = DefaultLayout defAtom (ExprX layers a n t ^. Model)
-- type instance DefaultLayout defAtom (ANTLayout l a n t) = (a ~ defAtom, n ~ (), t ~ ())
--
--
-- -- TODO: rename vvv
-- type DefaultLayout' a t = DefaultLayout a (t ^. Layout)



-- type instance DefaultLayout (ANTLayout SimpleX a n t) = ANTLayout SimpleX () () ()



-- !!! Moze zamienic Generalizable na TF zwracajaca jakas wartosc lub Constraint?
class Generalizable a b
-- instance Generalizable t t' => Generalizable (Term3 t) (Term3 t')
-- instance (layers ~ layers', Generalizable layout layout') => Generalizable (ExprX2 layers layout) (ExprX2 layers' layout')
instance Generalizable a (Uniform Draft)

generalize :: Generalizable a b => a -> b
generalize = unsafeCoerce ; {-# INLINE generalize #-}





-- instance validates ... => Generalizable (ExprX layers a n t) (ExprX layers a' n' t') where generalize = unsafeCoerce ; {-# INLINE generalize #-}


-- Universal powinno podmieniac layout na UniversalModel. Z takim layoutem mozemy wartosci wkaldac do grafu
-- po wlozeniu wartosci do grafu mozemy je odczytywac i przetwarzac, bo znamy ich Typy. Mozemy tez
-- je rzutowac na inne layoute, ktore pokrywaja sie z Universal (czyli wszysktie polaczenia sa draftami)
-- Dzieki uzywaniu layoutu Universal upraszczaja sie nam typy, np. Dowolny Expr przeniesiony na Universalma layout
-- UniversalModel i nie musi byc robiona mapa po Assocs szczegolnie jezeli bedizemy chieli supportowac nieznane klucze
-- Dodatkowo, trzeba przejsc na DynamicM2, wtedy graf bedzie przechowywal informacje o wartosciach
-- i bedzie mozna go lepiej o nie odpytywac bez niebezpeicznego rzutowania, poniewaz wszystkie wartosci tam
-- beda w UniversalModel,,,,,,,

-- type family Universal a
-- type instance Universal I = I
--
-- type Universal' a = Set Model (Universal (a ^. Model)) a
--
-- universal :: Term3 t -> Universal (Term3 t)
-- universal = unsafeCoerce ; {-# INLINE universal #-}
--
--
-- type instance Universal (Compound l as) = Compound l (List.ReplaceVals Draft as)
-- type instance Universal (ExprX ls a n t) = Universal' (ExprX ls a n t)


-- class Bindable3 t m a where
--     bind3 :: a -> m (Binder a t a)

newtype NodeRef     tgt = NodeRef (Ref2 Node tgt) deriving (Show)
newtype EdgeRef src tgt = EdgeRef (Ref2 Edge (Arc2 (Binding src) (Binding tgt))) deriving (Show)

makeWrapped ''NodeRef
makeWrapped ''EdgeRef

type instance Binder Net     = NodeRef
type instance Linker Net Net = EdgeRef


-- instance (MonadBuilder g m, DynamicM2 Node g m a)
--       => Bindable Net a m where
--     bind a = construct' a ; {-# INLINE bind #-}
--






-- universalBind2 :: Constructor' m (Binding (AnyExpr t layers))
--                => Expr t layers layout -> m (Binding (Expr t layers layout))
-- universalBind2 = unsafeUniversalAppM construct'

-- universalBind3 :: Bindable (AnyExpr t layers) m
--                => Expr t layers layout -> m (Binding (Expr t layers layout))
-- universalBind3 = unsafeUniversalAppM bind

-- universalBind3 :: (BindableX (AnyExpr t layers) m, Functor m)
--                => Expr t layers layout -> m (Binding (Expr t layers layout))
-- universalBind3 = unsafeUniversalAppM bindX

-- universalBind4 :: (Bindable t m, Functor m)
--                => Expr t layers layout -> m (Binding (Expr t layers layout))
-- universalBind4 = unsafeUniversalAppM bind


-- unsafeUniversalAppM :: Functor m => (Universal a -> m (Universal b)) -> a -> m b
-- unsafeUniversalAppM f a = unsafeCoerce <$> (f $ universal a)

-- class IsExprX a
-- instance (v ~ Binding (Expr t layers layout), layers ~ '[]) => IsExprX v
-- type IsExprX' = TypeConstraint2 IsExprX



instance (Monad m, MonadBuilder g m, DynamicM3 Node g m, ReferableM Node g m)
      => Bindable Net m where
    mkBinder   = NodeRef <∘> construct' ; {-# INLINE mkBinder   #-}
    readBinder = readRef . unwrap'      ; {-# INLINE readBinder #-}


instance (Monad m, MonadBuilder g m, DynamicM3 Edge g m)
      => Linkable Net Net m where
    mkLinker a b = EdgeRef <$> construct' (Arc2 a b)

test_gr1 :: ( ExprBuilder_i t layers m, Linkable t t m
            , MonadIO m, Show (PrimExpr' t layers Star), Show (Binding (PrimExpr' t layers Star)))
         => m ()
test_gr1 = do
    sref <- star4
    -- t <- readBinding sref
    -- l <- mkLink sref sref
    --
    -- case' t of
    --     Symbol.Unify l r -> print 11
    --     Symbol.Star      -> case' t of
    --         Symbol.Unify l r -> print "hello"
    --         Symbol.Star      -> print "hello3xx"
    --
    --
    -- print "!!!"
    -- print t
    -- print sref
    return ()


test_g2 :: forall m . (PrimMonad m, MonadIO m)
        => m ((), Network3 m)
test_g2 = do
    g <- NEC.emptyHMGraph
    flip Graph.Builder.runT g $ suppressAll
                              $ runInferenceT2 @InfLayers @'[]
                              $ runInferenceT2 @TermType  @Net
                              $ test_gr1
-- runInferenceT2 :: forall t cls m a. cls -> KnownTypeT cls t m a -> m a


newtype instance Layer t Type = TypeLayer (Unwrapped (Layer t Type))

instance Wrapped (Layer t Type) where
    type Unwrapped (Layer t Type) = SubLink Type t
    _Wrapped' = iso (\(TypeLayer l) -> l) TypeLayer ; {-# INLINE _Wrapped' #-}

deriving instance Show (Unwrapped (Layer t Type)) => Show (Layer t Type)


-- (Layer (Expr Net '[Type] (Prim () Star)) Type)

newtype instance Layer2 t layers layout Type = TypeLayer2 (SubLink Type (Expr t layers layout))

-- magicStar :: (LayersCons2' t layers m, Bindable t m) => m $ Binding (Expr t layers layout)

-- instance (Monad m, Bindable t m, Linkable' t m, LayersCons2' t layers m)
--       => LayerCons2 Type t layers m where
--     consLayer2 = do
--         s <- magicStar
--         l <- mkLink (error "x") s
--         return $ TypeLayer2 l

-- instance (Monad m, Linkable' t m, Top m t layers)
--       => LayerCons2 Type t layers m where
--     consLayer2 = do
--         s <- localTop
--         l <- mkLink (error "x") s
--         return $ TypeLayer2 l


instance (Monad m, Bindable t m, Linkable' t m, LayersCons2' t layers m)
      => LayerCons3 Type m (Expr t layers layout) where
    consLayer3 = do
        s <- magicStar
        l <- mkLink (error "x") s
        return $ TypeLayer l



    -- magicStar :: (LayersCons2' t layers m, Bindable t m) => m $ Binding (Expr t layers layout)
    -- magicStar = mkBinding =<< expr (wrap' N.star')

-- get type (getTop) moze byc zawsze draftem
-- selfconnect moze byc zahardcodowane w tworzeniu

-- data family Layer2 t (layers :: [*]) layout l
--
-- class LayerCons2 l t layers m where
--     consLayer2 :: forall layout. m (Layer2 t layers layout l)


    -- problemem jest to ze zarowno self jak i getType musza miec w typei cos zwiazanego z layout
    -- co jest tylko po to by sprawdzic czy sie matchuje, a nie chcemy do konsturowania warstw tego wrzucac
    -- bo nam bardzo typy to psuje
    --
    -- mozna zrobic classe SelfConnect - ktora moglaby to rozwiazywac
    -- ogolnie - mozna tworzyc typeclassy ktore gwarantowalyby te wlasnosci jakos
    --
    -- to jedna nie zadziala, bo typy nie beda sie matchowaly, bo consLayer ma forall layout.
    --
    -- Mozna za to zrobic conslayer kotry nie ma ZADNEGO foralla i ograniczyc typy
    -- dopiero w typeclassie pozniej, ktora nakladalaby informacje na layout, takie ze dzialalby
    -- self i getType, a nie musialby byc w kontekscie
    --
    -- -- self :: m $ Binding (Expr t layers layout)
    -- -- getType :: m $ Binding (Expr t layers (Sub Type layout))

data TopLayout = TopLayout deriving (Show)

type instance Get Atom TopLayout = TopLayout
type instance Atoms TopLayout = '[Star]

-- newTest :: IO (Expr2 Net '[] TopLayout)
-- newTest = do
--     e <- expr2
--     print e
--     return e
-- LayersCons2' t layers m, ValidateLayout layout Atom Star

-- nstar0 :: Constructor (ExprSymbol Star (Expr2 t ls layout)) m (TermStack2 t ls layout) => m (Expr2 t ls layout)
-- nstar0 = expr2 N.star'
type instance ExprLayer Type t = SubLink Type t

--                 --
--
class Monad m => MonadTop t layers m | m -> t layers where
    checkTop :: forall layout. m (Maybe (Binding (Expr2 t layers layout)))
    newTop   :: forall layout. m (Binding (Expr2 t layers layout))
    setTop   :: forall layout. Maybe (Binding (Expr2 t layers layout)) -> m ()


-- localTop :: MonadTop t layers m => m (Binding (Expr2 t layers layout))
-- localTop = checkTop >>= fromMaybeM (newTop >>~ setTop)

-- localTop :: (MonadTop t layers m, MonadFix m) => m (Binding (Expr2 t layers AnyLayout))
-- localTop = do
--     mt <- checkTop
--     case mt of
--         Just t  -> return t
--         Nothing -> mdo
--             setTop (Just nt)
--             nt <- newTop
--             setTop mt
--             return nt

newtype TopStore t layers m a = TopStore (StateT (Maybe (Binding (Expr2 t layers AnyLayout))) m a) deriving (Monad, Applicative, Functor, MonadTrans, MonadFix, MonadIO)
--

instance PrimMonad m => PrimMonad (TopStore t layers m) where
    type PrimState (TopStore t layers m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}
--
instance (Monad m, Constructor TermStore m (TermStack2 t layers AnyLayout)) => MonadTop t layers (TopStore t layers m) where
    checkTop = undefined -- TopStore State.get
    newTop   = undefined
    setTop   = undefined

topevalT (TopStore s) = State.evalStateT s


-- Type.get >>= fromMaybeM ()

type instance Get Atom AnyLayout = AnyLayout
type instance Atoms AnyLayout = '[Star]
--
localTop :: ( Type.MonadTypeBuilder (Binding (Expr2 t layers AnyLayout)) m, Constructor TermStore m (TermStack2 t layers AnyLayout)
            , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m, MonadFix m, Bindable t m)
         => m (Binding (Expr2 t layers AnyLayout))
localTop = do
    mt <- Type.get
    case mt of
        Just t  -> return t
        Nothing -> mdo
            Type.put (Just nt)
            nt <- nstar
            Type.put mt
            return nt


instance ( expr ~ Expr2 t layers AnyLayout
         , bind ~ Binding expr
         , Linkable' t m
         , Self.MonadSelfBuilder bind m

        --  , MonadTop t layers m
         , Type.MonadTypeBuilder bind m
         , Constructor TermStore m (TermStack2 t layers AnyLayout), t ~ Net, Bindable t m, MonadFix m
         ) => Constructor TermStore m (Layer4 (Expr2 t layers AnyLayout) Type) where
    cons _ = do
        -- undefined
        -- s <- nmagicStar
        -- Just tp <- Type.get
        self <- Self.get
        top  <- localTop -- to nie powinno byc localTop ( tak nie dziala z kontekstem z linii 576) - tylko powinno byc cos ala 589
        -- l    <- mkLink self (specifyLayout2 top)
        l    <- mkLink self top
        -- l    <- mkLink self self
        return $ Layer4 l

-- nstar1 :: (Monad m) => m (Expr2 Net '[Data] (ANTLayout SimpleX Star () ()))
-- nstar1 = expr3 N.star'

type ExprCons t layers m = Constructor TermStore m (TermStack2 t layers AnyLayout)

nstar1 :: (ExprCons t layers m, ValidateLayout layout Atom Star) => m (Expr2 t layers layout)
nstar1 = expr3 N.star'


nstar2 :: ExprCons t layers m => m (PrimExpr2' t layers Star)
nstar2 = nstar1



nstar :: ( ExprCons t layers m, Bindable t m
         , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
         , ValidateLayout layout Atom Star)
      => m (Binding (Expr2 t layers layout))
nstar = Self.put . anyLayout2 =<<& (expr3 N.star' >>= mkBinding)

nstar3 :: ( ExprCons t layers m, Bindable t m
          , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
          , Inferable2 InfLayers layers m, Inferable2 TermType t m)
       => m (Binding (PrimExpr2' t layers Star))
nstar3 = nstar


nstar' :: ( ExprCons t layers m, Bindable t m
         , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
         , Inferable2 Layout    layout m
         , Inferable2 InfLayers layers m
         , Inferable2 TermType  t      m
         )
      => m (Binding (Expr2 t layers (Set Atom Star layout)))
nstar' = Self.put . anyLayout2 =<<& (expr3 (wrap' N.star') >>= mkBinding)




type instance MatchLayouts (Prim () Star) (Prim () Star) = Prim () Star

type instance Specialized Atom s (Prim n a) = Prim n (s :> a)

data XX

-- nunify2 :: (MonadFix m, layout ~ Specialized Atom Unify (Merge l1 l2), ValidateLayout layout Atom Unify, Bindable t m, Linkable' t m, ExprCons t ls m)
--         => Binding (Expr2 t ls l1) -> Binding (Expr2 t ls l2) -> m (Binding (Expr2 t ls layout))
-- nunify2 a b = mdo
--     n  <- mkBinding =<< (expr3 $ N.unify' la lb)
--     la <- mkLink n (unsafeCoerce a) -- Sub Atom layout ~~ l1  -- should be checked
--     lb <- mkLink n (unsafeCoerce b)
--     return n

nunify :: ( MonadFix m
           , layout ~ Specialized Atom Unify (Merge l1 l2)
        --    , ValidateLayout layout Atom Unify
           , Bindable t m
           , Linkable' t m
           , ExprCons t layers m
           , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
           )
        => Binding (Expr2 t layers l1) -> Binding (Expr2 t layers l2) -> m (Binding (Expr2 t layers layout))
nunify a b = Self.put . anyLayout2 =<<& mdo
    n  <- mkBinding =<< (expr3 $ wrap' $ N.unify' la lb)
    la <- mkLink n (unsafeGeneralize a)
    lb <- mkLink n (unsafeGeneralize b)
    return n


instance {-# INCOHERENT #-} Constructor a m c => Constructor a (KnownTypeT cls t m) c where
    cons = lift . cons


instance {-# INCOHERENT #-} Linkable l l' m => Linkable l l' (KnownTypeT cls t m) where
    mkLinker    = lift .:  mkLinker    @l @l' -- :: forall a b. Binding a -> Binding b -> m (Linker t t' a b)
    rmLinker    = lift .   rmLinker    @l @l' -- :: forall a b. Linker t t' a b -> m ()
    writeLinker = lift .:. writeLinker @l @l' -- :: forall a b. Linker t t' a b -> Binding a -> Binding b -> m ()
    readLinker  = lift .   readLinker  @l @l' -- :: forall a b. Linker t t' a b -> m (Binding a, Binding b)

-- instance {-# INCOHERENT #-} Bindable b m => Bindable b (KnownTypeT cls t m)

instance {-# INCOHERENT #-} Bindable b m => Bindable b (KnownTypeT cls t m) where
    mkBinder    = lift .  mkBinder    @b -- :: forall a. a -> m (Binder t a)
    rmBinder    = lift .  rmBinder    @b -- :: forall a. Binder t a      -> m ()
    writeBinder = lift .: writeBinder @b -- :: forall a. Binder t a -> a -> m ()
    readBinder  = lift .  readBinder  @b -- :: forall a. Binder t a      -> m a



type family   UnsafeGeneralizable a b :: Constraint
type instance UnsafeGeneralizable (Binding a) (Binding b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Expr2 t layers l1) (Expr2 t layers l2) = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce

-- class Generalizable a b where
--     generalize :: a -> b

nmagicStar :: (ExprCons t layers m, Bindable t m) => m $ Binding (Expr2 t layers layout)
nmagicStar = mkBinding =<< expr3 (wrap' N.star')

-- star3' :: ExprBuilder2 t layers m => m $ Binding (PrimExpr' t layers Star)
-- star3' = universalDispatch Node =<< universalBind3' =<< star2
--
-- star4 :: ExprBuilder_i t layers m => m $ Binding (PrimExpr' t layers Star)
-- star4 = star3 -- inferTerm star3

            -- ntest :: (Monad m, MonadBuilder g m, DynamicM3 Node g m, DynamicM3 Edge g m, ReferableM Node g m) => m (Expr2 Net '[Data] (ANTLayout SimpleX Star () ()))
            -- ntest = nstar1

type ANT a n t = ANTLayout SimpleX a n t

type Expr' cls layers a n t = Expr2 cls layers (ANT a n t)
type UntyppedExpr cls layers a n = Expr' cls layers a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout

test_gr3 :: forall t layers m .
            ( MonadIO m
            , Bindable  t m
            , Linkable' t m
            , ExprCons t layers m
            , Inferable2 InfLayers layers m
            , Inferable2 TermType  t      m
            , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
            , Show (PrimExpr2' t layers Star), HasLayer Data layers
        ) => m ()
test_gr3 = baseLayout @(ANTLayout SimpleX () () Star) $ do
    (s1 :: Binding (UntyppedExpr t layers Star            ())) <- nstar'
    (s2 :: Binding (UntyppedExpr t layers Star            ())) <- nstar'
    (u1 :: Binding (UntyppedExpr t layers (Unify :> Star) ())) <- nunify s1 s2
            -- un   <- nunify2 s1 s1
    -- let x = s1 :: Binding (Expr2 t layers (ANTLayout SimpleX Star () Star))
    -- let _ = u1 :: _ -- :: Binding (Expr2 t layers (ANTLayout SimpleX Star () Star))
    t <- readBinding s1
            -- -- l <- mkLink s1 s1
            -- --
    case' t of
        Symbol.Unify l r -> print 11
        Symbol.Star      -> case' t of
            Symbol.Unify l r -> print "hello"
            Symbol.Star      -> print "hello3xx"
    --
    -- let { exp = t
    -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
    --     ala = 11
    -- ;f2 = matchx $ \Symbol.Star       -> (print "hello2" )
    --    where ala = 11
    -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])
    -- --
    -- --
    -- -- --
    -- -- --
    -- print "!!!"
    -- print t
    -- print s1
    -- print $ get @Sym $ unwrap' $ get @Data t
    return ()





        -- runCase :: Expr t layers layout -> [Prim.Any -> out] -> out
        -- runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
        --     s   = unwrap' $ get @Sym $ unwrap' $ get @Data el
        --     idx = unwrap' $ get @Atom $ unwrap' $ get @Data el
        -- {-# INLINE runCase #-}

        --
        -- -- let { exp = e1
        -- -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
        -- --     ala = 11
        -- -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])
        --
-- case' x1 of
--     Symbol.Unify l r -> print 11
--     Symbol.Star      -> case' x1 of
--         Symbol.Unify l r -> print "hello"
--         Symbol.Star      -> print "hello3x"


test_g3 :: forall m. (PrimMonad m, MonadIO m, MonadFix m)
        => m ((), Network3 m)
test_g3 = do
    g <- NEC.emptyHMGraph
    flip Self.evalT undefined $
        flip Type.evalT Nothing $
        flip Graph.Builder.runT g -- $ suppressAll
                                  $ runInferenceT2 @InfLayers @'[Data, Type]
                                  $ runInferenceT2 @TermType  @Net
                                  $ (test_gr3)


main :: IO ()
main = do

    -- newTest

    (x,g) <- test_g3
    print x

    exitSuccess

    -- print blank
    print N.blank
    -- print $ (runIdentity (encodeStore blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    print $ (runIdentity (encodeStore N.blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    -- let e1  = (runIdentity (cons2 blank  ) :: Expr Network2 '[] '[Int] (Layout Static Draft) (Layout Static Draft))
    -- let e1   = (runIdentity (cons2 N.blank) :: Expr Network2 '[] '[Int] (Layout N.String Draft) (Layout N.String Draft))
    -- let e1'  = (runIdentity (cons2 N.blank) :: Term2 Network2 '[] '[Int] (Layout.Named N.String Draft))
    let -- e1  = (runIdentity (cons2 N.blank) :: Expr Network2 (Layout.Named N.String Draft))
        -- er1 = (runIdentity (cons2 N.blank) :: Ref Edge (Expr Network2 (Layout.Named Draft Draft)))
        -- u1  = (runIdentity (unify_x er1 er1) :: Ref Edge (Expr Network2 (Layout.Named Draft Draft)))

        -- xe1 = (runIdentity (consTerm N.blank) :: (Term2 Network2 (Layout.TNA Draft Draft Draft)))
        --
        -- fs1 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Value))
        -- fs2 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Draft))

        -- s1 = S2.star :: S2.Symbol Star Net

        -- ss1 = runIdentity (term N.blank') :: MyExpr2 '[] Draft Draft Draft


        -- fss1 = 0 :: Ref2 Edge (MyExpr2 '[] (Missing :> Draft) Draft Draft)
        -- fss2 = 0 :: Ref2 Edge (MyExpr2 '[] (App :> Draft) Draft Draft)

        x1 = runIdentity star1 :: Expr Net '[] (ANTLayout SimpleX Star () ())
        -- su1 = runIdentity (term $ N.unify' fss1 fss1) :: MyExpr2 '[] (Unify :> Value) Draft Draft

        -- uux = runIdentity $ unify_auto fss2 fss1 :: Int

        -- fu1 = runIdentity $ xunify fs1 fs2 :: Int


    print "==="
    (x,g) <- test_g2
    print x
    -- print g
    -- let e2  = (runIdentity (cons2 blank  ) :: Expr' Static Draft)
    -- let e2 = (runIdentity (cons2 N.blank) :: Expr' Static Draft)
    -- let es1 = (runIdentity (cons2 star) :: Ref Edge (Expr' Static Value))
    -- let eb1 = (runIdentity (cons2 blank) :: Ref Edge (Expr' Static Draft))
    -- let eu1 = (runIdentity (cons2 $ unify eb1 eb1) :: Expr' Static Draft)
    -- let eu2 = (runIdentity (cons2 $ unify es1 es1) :: Expr Static Value Static Draft)
    -- print e2

    -- print s1
    putStrLn ""
    print x1
    print $ get @Data x1
    print $ get @Sym $ unwrap' $ get @Data x1
    print $ unwrap' $ get @Atom $ unwrap' $ get @Data x1
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


    -- let xx = (let blank = 2 in blank)

    --
    -- let { exp = x1
    -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
    --     ala = 11
    -- ;f2 = matchx $ \Symbol.Star       -> (print "hello2" )
    --    where ala = 11
    -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])
    --         --
    --         -- -- let { exp = e1
    --         -- -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
    --         -- --     ala = 11
    --         -- -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])
    --         --
    -- case' x1 of
    --     Symbol.Unify l r -> print 11
    --     Symbol.Star      -> case' x1 of
    --         Symbol.Unify l r -> print "hello"
    --         Symbol.Star      -> print "hello3x"


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
