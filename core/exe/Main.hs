{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic, Connection)
import Data.Graph.Builders hiding (Linkable)
import Prologue            hiding (elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter)

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Old.Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder       (MonadBuilder)
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
-- import           Data.Graph.Builder.Ref                          as Ref
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
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
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
import Luna.Syntax.Term.Expr hiding (Data, cons, unify, star)

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
import Luna.Syntax.Term.Expr.Symbol (Sym)
import Control.Lens.Property hiding (Constructor)
import Luna.Syntax.Term.Expr.Format (Format, Sub)
import TH
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import Luna.Syntax.Term.Expr.Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)
import qualified Type.List as TList
import qualified Control.Monad.State as State
import Control.Monad.State hiding (get, set)

import System.Exit (exitSuccess)
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import qualified Luna.Syntax.Model.Network.Builder.Type as Type

import Control.Monad.ST
import Data.Reprx
import Luna.Pretty.Styles  (HeaderOnly(..))

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers



runCase :: Getter Data (Expr t layers layout) => Expr t layers layout -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    d   = unwrap' $ get @Data el
    s   = unwrap' $ get @Sym  d
    idx = unwrap' $ get @Atom d
{-# INLINE runCase #-}


matchx :: expr ~ Expr t layers layout => expr -> (ExprSymbol atom expr -> b) -> x -> b
matchx t f = rebind (f . uniExprTypes t) where
    rebind :: (a -> b) -> x -> b
    rebind f = f . unsafeCoerce ; {-# INLINE rebind #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}

--


type NetLayers   = '[Data, Type]
type Network3    = NEC.HGraph                '[Node, Edge, Cluster]
type MNetwork3 m = NEC.HMGraph (PrimState m) '[Node, Edge, Cluster]






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

data ANT
type ANTLayout l a n t = Compound l '[Atom := a, Name := n, Type := t]


-- === Instances === ---

-- DefaultLayout
type instance DefaultLayout ANT = ANTLayout SimpleX () () Star

-- Sub
type instance Sub Atom (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Atom a) n t
type instance Sub Name (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Type t) (Sub Type t) (Sub Type t)

-- Specialized
type instance Specialized Atom spec (ANTLayout l a n t) = ANTLayout l (Simplify (spec :> a)) n t



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
    mkBinder    = NodeRef <∘> construct' ; {-# INLINE mkBinder    #-}
    readBinder  = readRef  . unwrap'     ; {-# INLINE readBinder  #-}
    writeBinder = writeRef . unwrap'     ; {-# INLINE writeBinder #-}

instance (Monad m, MonadBuilder g m, DynamicM3 Edge g m)
      => Linkable Net Net m where
    mkLinker a b = EdgeRef <$> construct' (Arc2 a b)






--
-- class ReferableM r t m where
--     setRefM  :: Ref2 r a -> a -> t -> m t
--     viewRefM :: Ref2 r a      -> t -> m a


-- === Type Layer === --

type instance ExprLayer Type t = SubLink Type t


instance ( expr ~ AnyExpr t layers
         , bind ~ Binding expr
         , Linkable' t m
         , Self.MonadSelfBuilder bind m
         , Type.MonadTypeBuilder bind m
         , Constructor TermStore m (AnyExprStack t layers), t ~ Net, Bindable t m, MonadFix m
         ) => Constructor TermStore m (Layer (AnyExpr t layers) Type) where -- we cannot simplify the type in instance head because of GHC bug https://ghc.haskell.org/trac/ghc/ticket/12734
    cons _ = do
        self <- Self.get
        top  <- localTop
        l    <- mkLink self top
        return $ Layer l






-- | Run a function and use the result as a context type
mfixType :: (Type.MonadTypeBuilder a m, MonadFix m) => m a -> m a
mfixType f = mfix $ flip Type.with' f . Just


localTop :: ( Type.MonadTypeBuilder (Binding (AnyExpr t layers)) m, Constructor TermStore m (AnyExprStack t layers)
            , Self.MonadSelfBuilder (Binding (AnyExpr t layers)) m, MonadFix m, Bindable t m)
         => m (Binding (AnyExpr t layers))
localTop = Type.get >>= fromMaybeM (mfixType magicStar)




type AnyExprCons t layers m = Constructor TermStore m (AnyExprStack t layers)


magicStar :: ( AnyExprCons t layers m, Bindable t m
             , Self.MonadSelfBuilder (Binding (AnyExpr t layers)) m)
          => m (Binding (AnyExpr t layers))
magicStar = Self.put . anyLayout2 =<<& (expr N.star' >>= mkBinding)


star :: ( AnyExprCons t layers m, Bindable t m
        , Self.MonadSelfBuilder (Binding (AnyExpr t layers)) m
        , Inferable2 Layout    layout m
        , Inferable2 InfLayers layers m
        , Inferable2 TermType  t      m )
      => m (Binding (Expr t layers (Set Atom Star layout)))
star = Self.put . anyLayout2 =<<& (expr (wrap' N.star') >>= mkBinding)



unify :: ( MonadFix m
          , Bindable t m
          , Linkable' t m
          , AnyExprCons t layers m
          , Self.MonadSelfBuilder (Binding (AnyExpr t layers)) m
          )
        => Binding (Expr t layers l1) -> Binding (Expr t layers l2) -> m (Binding (Expr t layers (Specialized Atom Unify (Merge l1 l2))))
unify a b = Self.put . anyLayout2 =<<& mdo
    n  <- mkBinding =<< (expr $ wrap' $ N.unify' la lb)
    la <- mkLink n (unsafeGeneralize a)
    lb <- mkLink n (unsafeGeneralize b)
    return n


instance {-# INCOHERENT #-} Constructor a m c => Constructor a (KnownTypeT cls t m) c where
    cons = lift . cons

instance {-# INCOHERENT #-} Linkable l l' m => Linkable l l' (KnownTypeT cls t m) where
    mkLinker    = lift .:  mkLinker    @l @l' ; {-# INLINE mkLinker    #-}
    rmLinker    = lift .   rmLinker    @l @l' ; {-# INLINE rmLinker    #-}
    writeLinker = lift .:. writeLinker @l @l' ; {-# INLINE writeLinker #-}
    readLinker  = lift .   readLinker  @l @l' ; {-# INLINE readLinker  #-}

instance {-# INCOHERENT #-} Bindable b m => Bindable b (KnownTypeT cls t m) where
    mkBinder    = lift .  mkBinder    @b ; {-# INLINE mkBinder    #-}
    rmBinder    = lift .  rmBinder    @b ; {-# INLINE rmBinder    #-}
    writeBinder = lift .: writeBinder @b ; {-# INLINE writeBinder #-}
    readBinder  = lift .  readBinder  @b ; {-# INLINE readBinder  #-}



type family   UnsafeGeneralizable a b :: Constraint
type instance UnsafeGeneralizable (Binding a) (Binding b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Expr t layers l1) (Expr t layers l2) = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




nmagicStar :: (AnyExprCons t layers m, Bindable t m) => m $ Binding (Expr t layers layout)
nmagicStar = mkBinding =<< expr (wrap' N.star')



data LensM s t a b m = LensM { _lensGetter :: s -> m a
                             , _lensSetter :: b -> m t
                             }

type LensM' s a m = LensM s s a a m

--
--
--
-- newtype LensM t a m = LensM { _lensGetter :: a -> m (Get t a)
--                             , _lensSetter ::
--                             }

type ANT' a n t = ANTLayout SimpleX a n t

type Expr' cls layers a n t = Expr cls layers (ANT' a n t)
type UntyppedExpr cls layers a n = Expr' cls layers a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


layouted :: forall l m a. KnownTypeT Layout (DefaultLayout l) m a -> m a
layouted = baseLayout @(DefaultLayout l)


runNewGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> m (a, Network3)
runNewGraphT f = do
    mg       <- NEC.unsafeThaw2 (NEC.emptyHGraph)
    (a, mg') <- Graph.Builder.runT f mg
    g'       <- NEC.unsafeFreeze2 mg'
    return (a, g')

runGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m (a, Network3)
runGraphT f g = do
    mg       <- NEC.thaw2 g
    (a, mg') <- Graph.Builder.runT f mg
    g'       <- NEC.unsafeFreeze2 mg'
    return (a, g')

evalGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m a
evalGraphT f g = fst <$> runGraphT f g

execGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m Network3
execGraphT f g = snd <$> runGraphT f g

evalGraph :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> a
evalGraph f g = runST $ evalGraphT f g

execGraph :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> Network3
execGraph f g = runST $ execGraphT f g


test_g3 :: (MonadIO m, PrimMonad m, MonadFix m)
        => m (Binding (UntyppedExpr Net NetLayers Star ()), Network3)
test_g3 = runNewGraphT
        $ flip Self.evalT undefined
        $ flip Type.evalT Nothing
        $ runInferenceT2 @InfLayers @NetLayers
        $ runInferenceT2 @TermType  @Net
        $ test_gr


instance Connection (Binding (Expr Net layers layout)) ((->) Network3) where
    read  a   = evalGraph $ read a    ; {-# INLINE read  #-}
    write a t = execGraph $ write a t ; {-# INLINE write #-}

instance XBuilder (Expr Net layers Draft) ((->) Network3) where
    bindings g = runST $ do
        mg <- NEC.thaw2 g
        fmap (Binding . NodeRef . unsafeRefer) <$> viewPtrs mg
    {-# INLINE bindings #-}

bindings2 :: (XBuilder a m, a ~ Expr Net NetLayers Draft) => m [Binding a]
bindings2 = bindings

elements2 :: (XBuilder a m, a ~ Expr Net NetLayers Draft) => m [a]
elements2 = elements



test_gr :: forall t layers m .
            ( MonadIO m
            , Bindable  t m
            , Linkable' t m
            , AnyExprCons t layers m
            , Inferable2 InfLayers layers m
            , Inferable2 TermType  t      m
            , Self.MonadSelfBuilder (Binding (AnyExpr t layers)) m
            , HasLayer Data layers
            , Show (UntyppedExpr t layers Star ())
            -- , Show (Linker' t (UntyppedExpr t layers Star ()))
        ) => m (Binding (UntyppedExpr t layers Star ()))
test_gr = layouted @ANT $ do
    (s1 :: Binding (UntyppedExpr t layers Star            ())) <- star
    (s2 :: Binding (UntyppedExpr t layers Star            ())) <- star
    (u1 :: Binding (UntyppedExpr t layers (Unify :> Star) ())) <- unify s1 s2

    t <- read s1
    write s1 t

    let u1'  = generalize u1 :: Binding (UntyppedExpr t layers Draft ())
        -- u1'' =

    -- bs <- bindings
    -- Unify l r <- read u1
    -- tgt  <- targetM l
    -- src  <- sourceM l
    -- both <- read l

    case' t of
        Unify l r -> print 11
        Star      -> case' t of
            Unify l r -> print "hello"
            Star      -> print "hello3xx"
        _         -> print "not found"


    let { case_expr = t
    ;f1 = matchx case_expr $ \(ExprSymbol (Symbol.Unify l r)) -> print ala where
        ala = 11
    ;f2 = matchx case_expr $ \(ExprSymbol Symbol.Star)       -> (print "hello3" )
       where ala = 11
    } in $(testTH2 'case_expr [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])


    -- print "!!!"
    -- print t
    -- print s1
    -- print $ get @Sym $ unwrap' $ get @Data t
    return s1



main :: IO ()
main = do

    -- let (s,g) = runST test_g3
    (s1,g) <- test_g3

    putStrLn "\n\n---"

    let t  = read  s1 g
        g' = write s1 t g
        bs = bindings2 g
        os = elements2 g

    putStrLn $ reprStyled HeaderOnly t
    print t
    -- print os
    print $ map (reprStyled HeaderOnly) os

    putStrLn "\n\n---"

    print t
    let d   = unwrap' $ get @Data t
        s   = unwrap' $ get @Sym  d
        idx = unwrap' $ get @Atom d

    print d
    -- print s
    print idx
    return ()
