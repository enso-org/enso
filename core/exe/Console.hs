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
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoOverloadedStrings       #-} -- https://ghc.haskell.org/trac/ghc/ticket/12797

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic, Connection, Ref, Referable, Link, link, Succs, CONNECTION, Ref, read2)
import qualified Data.Graph as G
import Data.Graph.Builders hiding (Linkable)
import Prologue            hiding (typeRep, elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter, set, Setter', set')
import qualified Prologue as P

import           Control.Monad.Event     as Event
import qualified Control.Monad.Delayed    as Delayed
import           Control.Monad.Delayed    (Runner, MonadDelayed, Delayed, delayed)
import qualified Control.Monad.Writer     as Writer
import           Old.Data.Attr                (attr)
import           Data.Construction        hiding (Register, register)
import           Data.Container           (index_)
import           Data.Container           hiding (impossible, elems, elems', elemsM)
import           Data.Graph.Builder       (MonadBuilder)
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
-- import           Old.Data.Prop
-- import           Data.Record              hiding (Cons, Layout, cons, Value)
-- import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import           Data.Container.Hetero                           (Hetero(..), Any(..))
import qualified Data.Graph.Builder.Class                        as Graph
import qualified Data.Graph.Builder.Class                        as Graph.Builder
-- import           Data.Graph.Builder.Ref                          as Ref
import           Luna.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Pass.Inference.Importing       as Importing
import           Luna.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Pass.Utils.Literals            as LiteralsUtils
import           Old.Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Old.Luna.Compilation.Stage.TypeCheck                as TypeCheck
import qualified Old.Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Luna.Env.Env                                 as Env
import qualified Luna.IR.Library.Symbol                             as Symbol
import           Old.Luna.Runtime.Dynamics                           (Dynamics, Dynamic, Static)
import qualified Old.Luna.Runtime.Dynamics                           as Runtime
import           Old.Luna.Syntax.Model.Layer                         ((:<), (:<:))
import           Old.Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
-- import           Old.Luna.Syntax.Model.Network.Builder.Node          hiding (curry, star, star, blank, unify)
import qualified Old.Luna.Syntax.Model.Network.Builder.Node          as Old
import           Old.Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Old.Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Old.Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Old.Luna.Syntax.Model.Network.Class                 (Network)
import qualified Old.Luna.Syntax.Model.Network.Term                  as Netx
-- import           Old.Luna.Syntax.Term                               (OverBuilder, Layout_OLD, ExprRecord, overbuild, AnyExpr)
import qualified Old.Luna.Syntax.Term.Class                           as Term
import           Luna.IR.Term.Format

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library

import           Old.Luna.Syntax.Model.Network.Builder.Class ()
import qualified Old.Luna.Syntax.Model.Network.Builder.Class as XP
-- import           Old.Luna.Syntax.Model.Network.Builder.Term.Class (star, SymbolBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

-- import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.IR.Term hiding (Data, cons, unify, star, Readable)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.IR.Internal.IR as IR
import Luna.IR.Internal.IR hiding (Readable, Bind, Fields, (:=)) -- (Model, Name, All, cons2, Layout(..), Term, Term3, Data(Data), Network2, NetworkT, consTerm, unsafeConsTerm, term, Term2)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.IR.Term.Atom (Atoms)

import qualified Luna.IR.Term.Symbol as Symbol
import qualified Luna.IR.Term.Symbol.Named as Sym
import qualified Luna.IR.Term.Symbol.Named as Symbol
import Luna.IR.Term.Symbol (Sym)
import Data.Property
import Luna.IR.Term.Format (Format, Sub)
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import Luna.IR.Term.Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)
import qualified Type.List as TList
import qualified Control.Monad.State as State
import Control.Monad.State hiding (get, set, modify', modify)

import System.Exit (exitSuccess)
import qualified Old.Luna.Syntax.Model.Network.Builder.Self as Self
import qualified Old.Luna.Syntax.Model.Network.Builder.Type as Type

import Control.Monad.ST
import Data.Reprx
import Luna.IR.Repr.Styles  (HeaderOnly(..))
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Control.Monad.State.Dependent.Old as D
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Web.Browser (openBrowser)

import qualified Luna.Diag.Vis as Vis
import           Luna.Diag.Vis (MonadVis)
import qualified Data.Set as S
import Type.Container (Every)
import Luna.IR.Layer
import Luna.IR.Layer.Model

import qualified Luna.Pass.Class as Pass
import Luna.Pass.Class (Keys, Preserves, Pass, read, Readable)

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers





-- === UID layer === --

data UID = UID deriving (Show)

type instance LayerData UID t = Int64

instance (Monad m, D.MonadState UID (LayerData UID expr) m) => LayerCons UID m where
    consLayer _ = Layer <$> D.modify UID (\s -> (succ s, s))

-- FIXME [WD]: Remove when moving out from constructors
instance (D.MonadState UID (LayerData UID expr) m, Monad m)
      => Constructor a m (Layer expr UID) where
    cons _ = Layer <$> D.modify UID (\s -> (succ s, s))




-- === Model layer === --

instance Monad m => LayerCons Model m where
    consLayer = return . Layer ; {-# INLINE consLayer #-}



-- === Type layer === --

type instance LayerData Type t = SubLink2 Type t


instance ( Self.MonadSelfBuilder (Ref Expr') m
         , Type.MonadTypeBuilder (Ref AnyExpr) m
         , MonadFix m
         , SilentExprCons m
         , MonadDelayed m
         , Register New (Ref ExprLink') (Delayed m)
         , StackCons (Layers ExprLink' (Cfg m)) m
         , Referable ExprLink' (Cfg m) m
         , t ~ Cfg m
         ) => Constructor TermStore m (Layer (IR t AnyExpr) Type) where -- we cannot simplify the type in instance head because of GHC bug https://ghc.haskell.org/trac/ghc/ticket/12734
    cons _ = do
        self <- anyLayout3 <$> Self.get
        top  <- localTop
        l    <- delayedLink top self
        l'   <- mark' l
        return $ Layer l'




-- === Succs layer === --

data Succs = Succs deriving (Show)

-- FIXME[WD]: refactorme
type instance LayerData Succs a = S.Set (XYZ a)
type family XYZ a where XYZ (IR t a) = IR t (Ref (Universal a))

instance Monad m => LayerCons Succs m where
    consLayer _ = return def ; {-# INLINE consLayer #-}

instance Monad m => Constructor a m (Layer expr Succs) where
    cons _ = Layer <$> return def ; {-# INLINE cons #-}


instance ( IRMonad m
         , IRReferables m
         , HasLayerM  m ExprLink' Model
         , HasLayersM m Expr' '[UID, Succs]
         , Ord (IRM m (Ref (Expr Draft)))
         , Eq  (IRM m (Ref (Expr Draft)))
         )
      => Handler Succs New (Ref ExprLink') m where
    handle linkRef = do
        (srcRef, tgtRef) <- select @Model =<< readx linkRef
        modifyx srcRef $ with @Succs (S.insert tgtRef)



type Network3    = NEC.HGraph                '[Node, Edge, Cluster]
type MNetwork3 m = NEC.HMGraph  '[Node, Edge, Cluster] (PrimState m)
type MNetworkX   = NEC.HMGraph  '[Node, Edge, Cluster]





data SimpleX




type family Specialized t spec layout






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
-- === Netx === --
-----------------

-- === Definition === --

data Netx = Netx

type family AllLayers a :: [*]
type instance Layers Expr'     Netx = '[Model, UID, Type, Succs]
type instance Layers ExprLink' Netx = '[Model, UID]
type instance AllLayers Netx = '[Model, UID, Type, Succs]

type instance Impl Ref Expr' Netx = G.Ref2 Node
type instance Impl Ref ExprLink' Netx = G.Ref2 Edge


------------

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Node g m, ReferableM Node g m, IRMonad m)
      => Referable Expr' Netx m where
    refDesc   = fromDefinition <=< construct' <=< liftIR ; {-# INLINE refDesc   #-}
    readDesc  = readRef  . view definition                ; {-# INLINE readDesc  #-}
    writeDesc = writeRef . view definition                ; {-# INLINE writeDesc #-}

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Edge g m, ReferableM Edge g m, IRMonad m)
      => Referable ExprLink' Netx m where
    refDesc   = fromDefinition <=< construct' <=< liftIR ; {-# INLINE refDesc   #-}
    readDesc  = readRef  . view definition                ; {-# INLINE readDesc  #-}
    writeDesc = writeRef . view definition                ; {-# INLINE writeDesc #-}


-- valOnly v = (Just' v, Nothing')
-- noVal   _ = (Nothing', Nothing')

-- fooe (x,y) = (Just' x, Just' y)








-- | Run a function and use the result as a context type
mfixType :: (Type.MonadTypeBuilder a m, MonadFix m) => m a -> m a
mfixType f = mfix $ flip Type.with' f . Just

--
localTop :: ( Type.MonadTypeBuilder (Ref AnyExpr) m, SilentExprCons m, Self.MonadSelfBuilder (Ref Expr') m )
         => m (Ref AnyExpr)
localTop = Type.get >>= fromMaybeM (mfixType magicStar)





magicStar :: (SilentExprCons m, Self.MonadSelfBuilder (Ref Expr') m)
          => m (Ref AnyExpr)
magicStar = Self.put . universal =<<& (silentExpr Sym.uncheckedStar)

-- expr' :: (ExprCons' m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Expr layout)



type l <+> r = Merge l r
type l :>> r = Specialized Atom l r



type IREvents m = ( Register New (Ref Expr')     (Delayed m)
                   , Register New (Ref ExprLink') (Delayed m)
                   )

type IRReferables m = ( Referable' m Expr'
                       , Referable' m ExprLink'
                       , ExprStore' m
                       )

type IRCons m = ( Self.MonadSelfBuilder (Ref Expr') m
                 , ExprBuilder m
                 , Linkable' Expr' Expr' m
                 )

type ASGBuilder m = ( IRMonad      m
             , IRReferables m
             , IRBaseLayers m
             , IREvents     (Runner m)
             , IRCons       (Runner m)
             , IRReferables (Runner m)
             )

type IRBaseLayers m = ( HasLayerM m Expr'     Model
                       , HasLayerM m ExprLink' Model
                       )


type ASGBuilder' m layout = (ASGBuilder m, Inferable2 Layout layout (Runner m))

unify :: ASGBuilder m => Ref (Expr l) -> Ref (Expr r) -> m (Ref (Expr (Unify :>> (l <+> r))))
-- unify :: ASGBuilder m => Expr l -> Expr r -> m (Expr (Unify :>> (l <+> r)))
unify a b = buildElem $ mdo
    n  <- delayedExpr (Sym.uncheckedUnify la lb)
    la <- delayedLink (unsafeGeneralize a) n
    lb <- delayedLink (unsafeGeneralize b) n
    return n


type AtomicExpr atom layout = Expr (Update Atom atom layout)
type AtomicExpr2 atom layout = Expr2 (Update Atom atom layout)

star :: ASGBuilder' m layout => m (Ref $ AtomicExpr Star layout)
star = buildElem $ expr Sym.uncheckedStar


star2 :: (IRMonad m, Inferable2 Layout layout m) => m (AtomicExpr2 Star layout)
star2 = expr2 Sym.uncheckedStar



-- expr2 :: (SymbolEncoder atom, ExprBuilder2 m)
--       => ExprSymbol atom (Expr2 layout) -> m (Expr2 layout)
-- expr2 a = Expr2 <$> newElem @Expr2_ (encodeSymbol a)

buildElem :: forall m a. (Self.MonadSelfBuilder (Universal a) (Runner m), Monad m)
          => Runner m a -> m a
buildElem m = Delayed.eval' $ Self.put ∘ universal =<<& m ; {-# INLINE buildElem #-}




type family   UnsafeGeneralizable a b :: Constraint

type instance UnsafeGeneralizable (Ref a) (Ref b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Expr l) (Expr l') = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




type ANT' a n t = ANTLayout SimpleX a n t


type AntExpr a n t = Expr (ANT' a n t)
type UntyppedExpr a n = AntExpr a n Star

type AntExpr2 a n t = Expr2 (ANT' a n t)
type UntyppedExpr2 a n = AntExpr2 a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
layouted = baseLayout @(DefaultLayout l)


runNewGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> m (a, Network3)
runNewGraphT f = do
    mg       <- NEC.unsafeThaw2 def
    (a, mg') <- Graph.Builder.runT f mg
    g'       <- NEC.unsafeFreeze2 mg'
    return (a, g')

runGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m (a, Network3)
runGraphT f g = do
    mg       <- NEC.thaw2 g
    (a, mg') <- Graph.Builder.runT f mg
    g'       <- NEC.unsafeFreeze2 mg'
    return (a, g')

runGraphTInplace :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m (a, Network3)
runGraphTInplace f g = do
    mg       <- NEC.unsafeThaw2 g
    (a, mg') <- Graph.Builder.runT f mg
    g'       <- NEC.unsafeFreeze2 mg'
    return (a, g')


evalGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m a
evalGraphT f g = fst <$> runGraphT f g

execGraphT :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m Network3
execGraphT f g = snd <$> runGraphT f g

evalGraphTInplace :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m a
evalGraphTInplace f g = fst <$> runGraphTInplace f g

execGraphTInplace :: PrimMonad m => GraphBuilder.BuilderT (MNetwork3 m) m a -> Network3 -> m Network3
execGraphTInplace f g = snd <$> runGraphTInplace f g

evalGraph :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> a
evalGraph f g = runST $ evalGraphT f g

execGraph :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> Network3
execGraph f g = runST $ execGraphT f g

runGraph :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> (a, Network3)
runGraph f g = runST $ runGraphT f g

evalGraphInplace :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> a
evalGraphInplace f g = runST $ evalGraphTInplace f g

execGraphInplace :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> Network3
execGraphInplace f g = runST $ execGraphTInplace f g

runGraphInplace :: (forall s. GraphBuilder.BuilderT (MNetwork3 (ST s)) (ST s) a) -> Network3 -> (a, Network3)
runGraphInplace f g = runST $ runGraphTInplace f g


type ExprStore' m = ExprStore (Cfg m) m
class Monad m => ExprStore t m where
    exprs' :: m [IR t (Ref Expr')]
    -- links  :: m [Ref Expr

exprs :: (ExprStore' m, IRMonad m) => m [Ref Expr']
exprs = join . fmap (sequence . fmap liftIR) $ exprs' ; {-# INLINE exprs #-}

instance (Monad m, MonadBuilder g m, ReferableM Node g m) => ExprStore Netx m where
    exprs' = (view (from definitionFake) . unsafeRefer) <<∘>> viewPtrs =<< GraphBuilder.get ; {-# INLINE exprs' #-}



type IRShow m a = Show (IRM m a)





-- class Setter' t a where set' ::           Get t a -> a -> a

-- select :: forall p m a. (Selector2 p m (IR.Marked m a), Markable m a) => a -> m (Select2 p m (IR.Marked m a))
-- select = select2 @p <=< IR.mark ; {-# INLINE select #-}








-- === Events handler === --

class    Monad m                     => ProxyHandler layer event a m where proxyHandle :: Proxy event -> a -> m ()
instance ReHandle layer event op a m => ProxyHandler layer event a m where proxyHandle _ = handle @layer @op ; {-# INLINE proxyHandle #-}
type     ReHandle layer event op a m = (event ~ op a, Handler layer op a m, Monad m)

class                         Monad m => Handler layer (op :: * -> *) a m where handle :: a -> m ()
instance {-# OVERLAPPABLE #-} Monad m => Handler layer op             a m where handle _ = return () ; {-# INLINE handle #-}


-- === Atomatic listeners discovery === --

type family Listeners ls m where
    Listeners '[]       m = m
    Listeners (l ': ls) m = AnyListener (ProxyHandler l) (Listeners ls m)

instance (Monad (Listeners ls m), EventsHandler ls m)
      => EventsHandler (l ': ls) m where handleEvents = handleEvents @ls . listenAny @(ProxyHandler l) (proxyHandle @l) ; {-# INLINE handleEvents #-}
instance EventsHandler '[]       m where handleEvents = id ; {-# INLINE handleEvents #-}
class    EventsHandler ls        m where handleEvents :: Listeners ls m a -> m a

type LayerListeners     m = Listeners     (AllLayers (Cfg m)) m
type LayerEventsHandler m = EventsHandler (AllLayers (Cfg m)) m

handleLayerEvents :: forall m a. LayerEventsHandler m => LayerListeners m a -> m a
handleLayerEvents = handleEvents @(AllLayers (Cfg m))


-- === IRBuilder === --

type IRBuilderCtxStack t m = ( KnownTypeT IRType t
                              $ Type.TypeBuilderT (Ref AnyExpr)
                              $ Self.SelfBuilderT (Ref Expr')
                              $ AllSuppressorT
                              $ IRT t
                              $ m
                              )

type MonadIRBuilder t m = (PrimMonad m, LayerEventsHandler (IRBuilderCtxStack t m))
type IRBuilder      t m = LayerListeners (IRBuilderCtxStack t m)


-- type instance Layers Expr'     Netx = '[Model, UID, Type, Succs]
-- type instance Layers ExprLink' Netx = '[Model, UID]

runIRBuilder :: forall t m a. MonadIRBuilder t m
              => IRBuilder t m a -> m (IR t a)
runIRBuilder = runIRT
             . Event.suppressAll
             . flip Self.evalT (undefined :: Ref Expr')
             . flip Type.evalT (Nothing :: Maybe (Ref AnyExpr))
             . runInferenceT2 @IRType @t
             . handleLayerEvents
    -- where reg = def
    --           & registerLayer @Expr' @Model
    --           -- & registerLayer @Expr' @UID
    --           -- & registerLayer @Expr' @Type
    --           -- & registerLayer @Expr' @Succs
    --           -- & registerLayer @ExprLink' @Model
    --           -- & registerLayer @ExprLink' @UID



type MonadNetBuilder m = MonadIRBuilder Netx m
type NetBuilder      m = IRBuilder      Netx m

runNetBuilder :: MonadNetBuilder m => NetBuilder m a -> m (IR Netx a)
runNetBuilder = runIRBuilder @Netx ; {-# INLINE runNetBuilder #-}


instance Generalize (Compound SimpleX lst) Draft





test_g4 :: (MonadIO m, PrimMonad m, MonadFix m)
        => m ()
test_g4 = flip (D.evalT UID) (0 :: Int64) $ do
        (_, vis) <- Vis.newRunDiffT $ do
            (exprRef, g) <- runNewGraphT $ runNetBuilder test_gr2
            return ()
        let cfg = ByteString.unpack $ encode $ vis
        putStrLn cfg
        liftIO $ openBrowser ("http://localhost:8200?cfg=" <> cfg)
        return ()

        -- star2 :: (ExprBuilder2 m, Inferable2 Layout layout m) => m (AtomicExpr2 Star layout)

dataRep = typeRep @Model
exprRep = typeRep @Expr2_


-- unhide :: Hidden -> a





-- pass1 :: Pass SimpleAA m
-- pass1 e = do
--     s <- read succs e
--     ...

-- TODO: data -> model ?

instance Readable layer abs m => Readable layer abs (KnownTypeT cls t m) where
    read = lift . read @layer ; {-# INLINE read #-}




data                    SimpleAA
type instance Keys      SimpleAA = '[LayerKey 'RW Expr2_ Model]
type instance Preserves SimpleAA = '[]

pass1 :: IRMonad m => Pass SimpleAA m
pass1 = gen_pass1

pass1_r :: (PrimMonad m, MonadIO m) => m (Either Pass.Err ())
pass1_r = runIRT2 $ do
    registerElem  @Expr2_
    registerLayer @Model
    attachLayer   dataRep exprRep

    test_pass1


gen_pass1 :: (IRMonad m, Readable Model Expr2_ m) => m ()
gen_pass1 = layouted @ANT $ do
    (s1 :: UntyppedExpr2 Star ()) <- star2
    (s2 :: UntyppedExpr2 Star ()) <- star2
    d <- read @Model s1
    return ()


test_pass1 :: IRMonad m => m (Either Pass.Err ())
test_pass1 = Pass.eval pass1


test_gr2 :: ( ASGBuilder (Layouted ANT m)
            , HasLayerM  m ExprLink' UID
            , HasLayersM m Expr'     '[Type, UID]
            , IRShow m (UntyppedExpr Star ())
            , MonadVis m
            , MonadIO m

            , IRMonad m
            , PrimState (GetIRMonad m) ~ PrimState m -- FIXME
            , PrimMonad m -- FIXME
            )
         => m (Ref (UntyppedExpr Star ()))
test_gr2 =  layouted @ANT $ do
    registerElem  @Expr2_
    registerLayer @Model
    attachLayer   dataRep exprRep

    (sx1 :: UntyppedExpr2 Star ()) <- star2
    (sx2 :: UntyppedExpr2 Star ()) <- star2
    -- Just (data_ :: Key m RW Expr2_ Model) <- lookupKey exprRep dataRep
    Just (data_ :: LayerKey RW Expr2_ Model) <- uncheckedLookupKey
    print sx1
    print sx2
    print =<< readKey data_ sx1
    (s1 :: Ref (UntyppedExpr Star            ())) <- star
    snapshot "s1"
    (s2 :: Ref (UntyppedExpr Star            ())) <- star
    snapshot "s2"
    (u1 :: Ref (UntyppedExpr (Unify :> Star) ())) <- unify s1 s2
    snapshot "s3"
    u2 <- unify s1 u1
    snapshot "s4"
    u3 <- unify s2 u1
    snapshot "s5"
    u4 <- unify u2 u3
    snapshot "s6"

    t <- readx s1
    s1' <- IR.mark' t
    -- Just dkey <- askKey @'RW @Expr' @Model
    print "!!!!!!!!----"
    -- print (unwrap' dkey)
    d <- select @Model t
    print s1'

    matchM t $ \case
        Unify l r -> print "ppp"
        Star      -> matchM t $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"


    return s1

-- matchy :: IRMonad m => a -> f -> m (IRM m a)
-- matchy t f = IR.mark' t
-- matchy t f = IR.mark' t >>= (exprUniSymbol . f)

matchy :: HasLayer t Expr' Model => (IR t (Expr layout)) -> (Unwrapped (ExprUniSymbol (Expr layout)) -> b) -> b
matchy a f = f $ unwrap' (exprUniSymbol a)

matchM :: (HasLayerM m Expr' Model, IRMonad m) => Expr layout -> (Unwrapped (ExprUniSymbol (Expr layout)) -> m b) -> m b
matchM a f = mark' a >>= flip matchy f

snapshot :: Vis m => P.String -> m()
snapshot title = do
    res <- exprs
    es  <- mapM readx res
    vss <- mapM visNode2 es
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves



main :: IO ()
main = do
    test_g4
    p <- pass1_r
    print p
    return ()


type Vis m = ( ASGBuilder m
             , MonadVis m
             , HasLayersM m Expr'     '[UID, Type]
             , HasLayerM  m ExprLink' UID
             )
visNode2 :: Vis m => Expr' -> m (Vis.Node, [Vis.Edge])
visNode2 expr = do
    mexpr  <- mark' expr
    euid   <- select @UID expr
    tpRef  <- select @Type expr
    tpLink <- readx tpRef
    tpUid  <- select @UID  tpLink
    (l,r)  <- select @Model tpLink

    ln     <- readx l
    rn     <- readx r
    -- --
    lnUID <- select @UID ln
    rnUID <- select @UID rn

    let header = fromString $ reprStyled HeaderOnly mexpr
        node   = Vis.Node (fromString "") euid euid (fromList [header])
        ins    = symbolFields mexpr
        tpVis  = if lnUID == rnUID then [] else [Vis.Edge (fromString "") tpUid tpUid lnUID rnUID (fromList [fromString "type"])]
        mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty
        getUIDs re = do
            e      <- readx re
            i      <- select @UID  e
            (l, r) <- select @Model e
            ln     <- readx l
            rn     <- readx r
            lnUID  <- select @UID ln
            rnUID  <- select @UID rn
            return (i, lnUID, rnUID)

    uss <- mapM getUIDs ins

    let edges = tpVis <> (mkEdge <$> uss)
    return (node, edges)
