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






data SimpleX




type family Specialized t spec layout



newtype NodeRef     tgt = NodeRef (Ref2 Node tgt) deriving (Show)
newtype EdgeRef src tgt = EdgeRef (Ref2 Edge (Arc2 (Binding src) (Binding tgt))) deriving (Show)

makeWrapped ''NodeRef
makeWrapped ''EdgeRef



-----------------
-- === ANT === --
-----------------

-- === Definition === ---

type ANT l a n t = Compound l '[Atom := a, Name := n, Type := t]


-- === Instances === ---

-- Sub
type instance Sub Atom (ANT SimpleX a n t) = ANT SimpleX (Sub Atom a) n t
type instance Sub Name (ANT SimpleX a n t) = ANT SimpleX (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ANT SimpleX a n t) = ANT SimpleX (Sub Type t) (Sub Type t) (Sub Type t)

-- Specialized
type instance Specialized Atom spec (ANT l a n t) = ANT l (Simplify (spec :> a)) n t



-----------------
-- === Net === --
-----------------

-- === Definition === --

data Net = Net

type instance Binder Net     = NodeRef
type instance Linker Net Net = EdgeRef


-- === Instances === --

instance (Monad m, MonadBuilder g m, DynamicM3 Node g m, ReferableM Node g m)
      => Bindable Net m where
    mkBinder   = NodeRef <∘> construct' ; {-# INLINE mkBinder   #-}
    readBinder = readRef . unwrap'      ; {-# INLINE readBinder #-}


instance (Monad m, MonadBuilder g m, DynamicM3 Edge g m)
      => Linkable Net Net m where
    mkLinker a b = EdgeRef <$> construct' (Arc2 a b)








-- === Type Layer === --

type instance ExprLayer Type t = SubLink Type t


instance ( expr ~ Expr2 t layers AnyLayout
         , bind ~ Binding expr
         , Linkable' t m
         , Self.MonadSelfBuilder bind m
         , Type.MonadTypeBuilder bind m
         , Constructor TermStore m (TermStack2 t layers AnyLayout), t ~ Net, Bindable t m, MonadFix m
         ) => Constructor TermStore m (Layer4 (Expr2 t layers AnyLayout) Type) where
    cons _ = do
        self <- Self.get
        top  <- localTop
        l    <- mkLink self top
        return $ Layer4 l












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







type ExprCons t layers m = Constructor TermStore m (TermStack2 t layers AnyLayout)


nstar :: ( ExprCons t layers m, Bindable t m
         , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
         , ValidateLayout layout Atom Star)
      => m (Binding (Expr2 t layers layout))
nstar = Self.put . anyLayout2 =<<& (expr3 N.star' >>= mkBinding)


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


nunify :: ( MonadFix m
          , Bindable t m
          , Linkable' t m
          , ExprCons t layers m
          , Self.MonadSelfBuilder (Binding (Expr2 t layers AnyLayout)) m
          )
        => Binding (Expr2 t layers l1) -> Binding (Expr2 t layers l2) -> m (Binding (Expr2 t layers (Specialized Atom Unify (Merge l1 l2))))
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



nmagicStar :: (ExprCons t layers m, Bindable t m) => m $ Binding (Expr2 t layers layout)
nmagicStar = mkBinding =<< expr3 (wrap' N.star')



type ANT' a n t = ANT SimpleX a n t

type Expr' cls layers a n t = Expr2 cls layers (ANT' a n t)
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
test_gr3 = baseLayout @(ANT SimpleX () () Star) $ do
    (s1 :: Binding (UntyppedExpr t layers Star            ())) <- nstar'
    (s2 :: Binding (UntyppedExpr t layers Star            ())) <- nstar'
    (u1 :: Binding (UntyppedExpr t layers (Unify :> Star) ())) <- nunify s1 s2

    t <- readBinding s1

    case' t of
        Symbol.Unify l r -> print 11
        Symbol.Star      -> case' t of
            Symbol.Unify l r -> print "hello"
            Symbol.Star      -> print "hello3xx"

    -- print "!!!"
    -- print t
    -- print s1
    -- print $ get @Sym $ unwrap' $ get @Data t
    return ()




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

    (x,g) <- test_g3
    print x

    return ()
