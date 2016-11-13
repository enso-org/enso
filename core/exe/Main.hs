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
{-# LANGUAGE NoOverloadedStrings       #-} -- https://ghc.haskell.org/trac/ghc/ticket/12797
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic, Connection, Ref, Referable, Link, link, Succs, CONNECTION, Ref, read2)
import qualified Data.Graph as G
import Data.Graph.Builders hiding (Linkable)
import Prologue            hiding (elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter, set, Setter', set')

import           Control.Monad.Event2     as Event
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
import           Luna.Pretty.GraphViz2
import           Luna.Runtime.Dynamics                           (Dynamics, Dynamic, Static)
import qualified Luna.Runtime.Dynamics                           as Runtime
import           Luna.Syntax.Model.Layer                         ((:<), (:<:))
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
-- import           Luna.Syntax.Model.Network.Builder.Node          hiding (curry, star, star, blank, unify)
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
-- import           Luna.Syntax.Model.Network.Builder.Term.Class (star, SymbolBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

-- import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr hiding (Data, cons, unify, star)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.Syntax.Term.Expr.Class as Asg
import Luna.Syntax.Term.Expr.Class hiding (Bind, Fields, (:=)) -- (Model, Name, All, cons2, Layout(..), Term, Term3, Data(Data), Network2, NetworkT, consTerm, unsafeConsTerm, term, Term2)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

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
import Control.Monad.State hiding (get, set, modify', modify)

import System.Exit (exitSuccess)
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import qualified Luna.Syntax.Model.Network.Builder.Type as Type

import Control.Monad.ST
import Data.Reprx
import Luna.Pretty.Styles  (HeaderOnly(..))
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Control.Monad.State.Dependent as D
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Web.Browser (openBrowser)

import qualified Luna.Pretty.Graph as Vis
import qualified Data.Set as S
-- import Data.Ident


title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers

type ExprLink' = Link' Expr'


runCase :: Getter Data (Asg t (Expr layout)) => Asg t (Expr layout) -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    d   = get @Data el
    s   = unwrap' $ get @Sym  d
    idx = unwrap' $ get @Atom d
{-# INLINE runCase #-}


matchx :: expr ~ Expr layout => Asg t expr -> (ExprSymbol atom expr -> b) -> x -> b
matchx t f = rebind (f . uniExprTypes2 t) where
    rebind :: (a -> b) -> x -> b
    rebind f = f . unsafeCoerce ; {-# INLINE rebind #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}





-- === UID layer === --

data UID = UID deriving (Show)

type instance LayerData UID t = Int64

instance (Monad m, D.MonadState UID (LayerData UID expr) m) => LayerCons UID m where
    consLayer _ = Layer <$> D.modify UID (\s -> (succ s, s))

-- FIXME [WD]: Remove when moving out from constructors
instance (D.MonadState UID (LayerData UID expr) m, Monad m)
      => Constructor a m (Layer expr UID) where
    cons _ = Layer <$> D.modify UID (\s -> (succ s, s))




-- === Data layer === --

instance Monad m => LayerCons Data m where
    consLayer = return . Layer ; {-# INLINE consLayer #-}



-- === Type layer === --

type instance LayerData Type t = SubLink Type t


instance (
        --  , Linkable ExprLink' t m
        --  , Referable ExprLink' t m
        --  , Referable Elemx t m

           Self.MonadSelfBuilder (Ref Expr') m
         , Type.MonadTypeBuilder (Ref AnyExpr) m
        --  , Constructor TermStore m (AnyExprStack t)
         , MonadFix m
         , SilentExprCons m
         , MonadDelayed m
         , Register New (Ref ExprLink') (Delayed m)
         , StackCons (Layers ExprLink' (Cfg m)) m
         , Referable ExprLink' (Cfg m) m

         ) => Constructor TermStore m (Layer AnyExpr Type) where -- we cannot simplify the type in instance head because of GHC bug https://ghc.haskell.org/trac/ghc/ticket/12734
    cons _ = do
        self <- anyLayout3 <$> Self.get
        top  <- localTop
        l    <- delayedLink top self
        -- dispatch @(New CONNECTION) (universal la)
        return $ Layer l




-- === Succs layer === --

data Succs = Succs deriving (Show)

type instance LayerData Succs t = S.Set (Ref (Universal t))

instance Monad m => LayerCons Succs m where
    consLayer _ = return def ; {-# INLINE consLayer #-}

instance Monad m => Constructor a m (Layer expr Succs) where
    cons _ = Layer <$> return def ; {-# INLINE cons #-}


instance ( MonadIO m
         , AsgMonad m
         , HasLayerM  m ExprLink' Data
         , HasLayersM m Expr' '[UID, Succs]
         , Referable ExprLink' (Cfg m) m
         , Referable Expr'     (Cfg m) m
        --  , HasLayer Elemx t UID
        --  , HasLayer Elemx t Succs
        --  , HasLayer ExprLink' t Data
        --  , Show (Expr t Draft)
         , a ~ Ref ExprLink'
        --  , Referable Elemx t m
        --  , Referable ExprLink' t m
        --  , Ord (Ref (Expr t Draft))
        --  , Eq  (Ref (Expr t Draft))
        --  , Selector Data m ExprLink'
         )
      => Handler Succs (New (Ref ExprLink')) a m where
    handle _ linkRef = do
        (srcRef, tgtRef) <- select @Data =<< read linkRef
        modify srcRef $ \src -> do
            succs <- select @Succs src
            -- update @Succs (S.insert tgtRef succs) src
            return src
        src <- read srcRef
        su  <- select @UID src
        print su
        print "connection"

-- update :: forall p m a. Updater p m a => Select p m a -> a -> m (AsgVal a)

-- select :: forall p m a. Selector p m a => a -> m (Select p m a)


type Network3    = NEC.HGraph                '[Node, Edge, Cluster]
type MNetwork3 m = NEC.HMGraph  '[Node, Edge, Cluster] (PrimState m)
type MNetworkX   = NEC.HMGraph  '[Node, Edge, Cluster]


-- type instance CfgX Network3 = Net -- Fixme, we should always wrap graph in some structure providing the `t` arg (in thos case t = Net)





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
-- === Net === --
-----------------

-- === Definition === --

data Net = Net

type instance Layers Expr'     Net = '[Data, UID, Type, Succs]
type instance Layers ExprLink' Net = '[Data, UID]

type instance Impl Ref Expr' Net = G.Ref2 Node
type instance Impl Ref ExprLink' Net = G.Ref2 Edge


-- === Instances === --

-- Refs



    -- instance Referable Elemx Net ((->) Network3) where
    --     refDesc  a t = fooe $ runGraph  (silentRef'  a) t ; {-# INLINE refDesc  #-}
    --     readDesc a t = evalGraphInplace (read' a) t ; {-# INLINE readDesc #-}
    --
    -- instance Referable ExprLink' Net ((->) Network3) where
    --     refDesc  a t = fooe $ runGraph  (silentRef'  a) t ; {-# INLINE refDesc  #-}
    --     readDesc a t = evalGraphInplace (read' a) t ; {-# INLINE readDesc #-}


-------


instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Node g m, ReferableM Node g m, AsgMonad m)
      => Referable Expr' Net m where
    refDesc   = fromDefinition <=< construct' <=< liftAsg ; {-# INLINE refDesc   #-}
    readDesc  = readRef  . view definition                ; {-# INLINE readDesc  #-}
    writeDesc = writeRef . view definition                ; {-# INLINE writeDesc #-}

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Edge g m, ReferableM Edge g m, AsgMonad m)
      => Referable ExprLink' Net m where
    refDesc   = fromDefinition <=< construct' <=< liftAsg ; {-# INLINE refDesc   #-}
    readDesc  = readRef  . view definition                ; {-# INLINE readDesc  #-}
    writeDesc = writeRef . view definition                ; {-# INLINE writeDesc #-}


valOnly v = (Just' v, Nothing')
noVal   _ = (Nothing', Nothing')

fooe (x,y) = (Just' x, Just' y)








-- | Run a function and use the result as a context type
mfixType :: (Type.MonadTypeBuilder a m, MonadFix m) => m a -> m a
mfixType f = mfix $ flip Type.with' f . Just

--
localTop :: ( Type.MonadTypeBuilder (Ref AnyExpr) m, SilentExprCons m, Self.MonadSelfBuilder (Ref Expr') m )
         => m (Ref AnyExpr)
localTop = Type.get >>= fromMaybeM (mfixType magicStar)


-- ASTFunc t (Expr layer)
-- ASTFunc t (Link' (Expr layer))
-- ASTFunc t (Group (Expr layer))
--
-- ASTFunc t (Ref (Expr layer))

-- type ASTEvents t m = ( Register New (Ref (Expr t Draft)) m
--                      , Register New (Ref (Link (Expr t Draft) (Expr t Draft))) m
--                      )
--
-- type ASTCons t m = ( ASTRefs t m
--                    , Self.MonadSelfBuilder (Ref (Expr t Draft)) m  -- TODO: parametrize it for ExprLink' / Expr / ...
--                    , Constructor TermStore m (AnyExprStack t)      -- TODO: remove and replace with linkable-like impl
--                    , Linkable ExprLink' t m
--                    )
--
-- type ASTRefs t m = ( Referable Elemx t m
--                    , Referable ExprLink' t m
--                    , TTT t m
--                    )
--
-- type ASTBaseData t = ( HasLayer Elemx     t Data
--                      , HasLayer ExprLink' t Data
--                      )
--
--
-- type ASTBuilder t m = ( MonadFix      m
--                       , NoOutput      m
--                       , ASTBaseData t
--                       , ASTEvents   t m
--                       , ASTRefs     t m
--                       , ASTCons t (Runner m)
--                       )
--
-- type SymbolBuilder t layout m = ( ASTBuilder t m
--                                 , Inferable2 Layout layout (Runner m)
--                                 )
--
--
--
-- type ASTFunc t ast = ( ASTRefs t ((->) ast)
--                  , ASTBaseData t
--                  )
--
--
--
--
-- type AnyExprCons t m = ( Constructor TermStore m (AnyExprStack t)
--                        )
--
-- type ASTPretty t = ( Show (AnyExpr t)
--                    , Show (Ref (AnyExpr t))
--                    )



magicStar :: (SilentExprCons m, Self.MonadSelfBuilder (Ref Expr') m)
          => m (Ref AnyExpr)
magicStar = Self.put . universal =<<& (silentExpr N.star')

-- expr' :: (ExprCons' m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Expr layout)



type l <+> r = Merge l r
type l :>> r = Specialized Atom l r


-- data CONNECTION



    -- type Referable'' a m = (Referable (Struct a) (Cfg2 a) m, Register New (Ref a) m)
    -- type RunnerReferable a n m = (Referable (Struct a) (Cfg2 a) m, Register New (Ref a) n, MonadDelayed n m)
    --
    -- ref2' :: Referable'' a m => a -> m (Ref a)
    -- ref2' a = do
    --     r <- silentRef' a
    --     register @New r
    --     return r

    -- delayedRef' :: RunnerReferable a n m => a -> m (Ref a)
    -- delayedRef' a = do
    --     r <- silentRef' a
    --     delayed $ register @New r
    --     return r




unify :: ( Register New (Ref Expr') m
         , Register New (Ref ExprLink') m
         , Self.MonadSelfBuilder (Ref Expr') m
         , Referable Expr' (Cfg m) (Runner m)
         , Referable ExprLink' (Cfg m) (Runner m)
         , AsgMonad (Runner m)
         , Constructor TermStore (Runner m) (Definition (Cfg m) AnyExpr)
         , AsgMonad (Runner m)
         , StackCons (Layers (Link Expr' Expr') (Cfg m)) (Runner m) -- linkable?
         )
       => Ref (Expr l) -> Ref (Expr r) -> m (Ref (Expr (Unify :>> (l <+> r))))
unify a b = buildElem $ mdo
    n  <- delayedExpr (wrap' $ N.unify' la lb)
    la <- delayedLink (unsafeGeneralize a) n
    lb <- delayedLink (unsafeGeneralize b) n
    return n


star :: ( AsgMonad (Runner m)
        , Constructor TermStore (Runner m) (Definition (Cfg m) AnyExpr)
        , expr ~ Expr (Set Atom Star layout)
        , ref  ~ Ref expr
        , Referable' expr (Runner m)
        , Self.MonadSelfBuilder (Universal ref) (Runner m)
        , Event (New (Universal ref)) m (Universal ref)
        , Inferable2 Layout layout (Runner m)
        )
      => m ref
star = buildElem $ expr (wrap' N.star')


buildElem :: forall m a. (Self.MonadSelfBuilder (Universal a) (Runner m), Monad m)
          => Runner m a -> m a
buildElem m = Delayed.eval' $ Self.put ∘ universal =<<& m ; {-# INLINE buildElem #-}




type family   UnsafeGeneralizable a b :: Constraint

type instance UnsafeGeneralizable (Ref a) (Ref b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Expr l) (Expr l') = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




    -- nmagicStar :: (AnyExprCons t m, Referable Elemx t m) => m $ Ref (Expr t layout)
    -- nmagicStar = silentExpr (wrap' N.star')


type ANT' a n t = ANTLayout SimpleX a n t


type AntExpr a n t = Expr (ANT' a n t)
type UntyppedExpr a n = AntExpr a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
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

-- test_g3 :: (MonadIO m, PrimMonad m, MonadFix m)
--         => m (Ref (UntyppedExpr Net Star ()), Network3)
-- test_g3 = runNewGraphT
--         $ Event.suppressAll
--         $ flip Self.evalT undefined
--         $ flip Type.evalT Nothing
--         $ runInferenceT2 @TermType @Net
--         $ flip (D.evalT UID) (0 :: Int64)
--         $ listenAny @(Handler Data ) (handle @Data )
--         $ listenAny @(Handler UID  ) (handle @UID  )
--         $ listenAny @(Handler Type ) (handle @Type )
--         $ listenAny @(Handler Succs) (handle @Succs)
--         -- $ Runner.eval'
--         $ test_gr


class Monad m => Handler layer event a m where handle :: Proxy event -> a -> m ()

instance {-# OVERLAPPABLE #-} Monad m => Handler layer event a m where handle _ _ = return () ; {-# INLINE handle #-}
-- instance {-# OVERLAPPABLE #-} Monad m => Handler Data event a m where handle _ _ = return () ; {-# INLINE handle #-}
-- instance {-# OVERLAPPABLE #-} Monad m => Handler Type event a m where handle _ _ = return () ; {-# INLINE handle #-}
-- instance {-# OVERLAPPABLE #-} Monad m => Handler Type event a m where handle _ _ = return () ; {-# INLINE handle #-}





        -- instance {-# OVERLAPPABLE #-} (Monad m, MonadBuilder g m, ReferableM Node g m, Inferable2 TermType Net m, NoOutput m) => TTT Net m where
        --     elems' = (Ref . unsafeRefer) <<∘>> viewPtrs =<< GraphBuilder.get
        --
        -- instance Inferable2 TermType Net ((->) Network3)
        -- instance TTT Net ((->) Network3) where
        --     elems' = evalGraphInplace $ runInferenceT2 @TermType @Net elemsM ; {-# INLINE elems' #-}
        --




-- test_gr :: forall t m. ( MonadIO m
--                        , ASTBuilder t (Layouted ANT m)
--                        , ASTPretty  t
--                        ) => m (Ref (UntyppedExpr t Star ()))
-- test_gr =  layouted @ANT $ do
--     (s1 :: Ref (UntyppedExpr t Star            ())) <- star
--     (s2 :: Ref (UntyppedExpr t Star            ())) <- star
--     (u1 :: Ref (UntyppedExpr t (Unify :> Star) ())) <- unify s1 s2
--     u2 <- unify s1 u1
--     u3 <- unify s2 u1
--     u4 <- unify u2 u3
--
--
--     t <- read s1
--     write s1 t
--
--     let u1'  = generalize u1 :: Ref (Expr t Draft)
--
--     es <- elems
--     es' <- mapM read es
--     --
--     -- case' t of
--     --     Unify l r -> print "ppp"
--     --     Star      -> case' t of
--     --         Unify l r -> print "hello"
--     --         Star      -> print "hello3xx"
--     --     _         -> print "not found"
--
--     --
--     -- let { case_expr = t
--     -- ;f1 = matchx case_expr $ \(ExprSymbol (Symbol.Unify l r)) -> print ala where
--     --     ala = 11
--     -- ;f2 = matchx case_expr $ \(ExprSymbol Symbol.Star)       -> (print "hello3" )
--     --    where ala = 11
--     -- } in $(testTH2 'case_expr [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])
--     --
--     print "!!!"
--     print t
--     print es
--     print es'
--     print s1
--     return s1

test_gr2 :: forall t m n expr ref.
         ( AsgMonad (Runner m)
         , Constructor TermStore (Runner m) (Definition (Cfg m) AnyExpr)
         , expr ~ UntyppedExpr Star ()
         , ref  ~ Ref expr
         , Referable' expr (Runner m)
         , Referable' expr m
         , Referable ExprLink' (Cfg m) (Runner m)
         , Self.MonadSelfBuilder (Universal ref) (Runner m)
         , Register New (Ref Expr') m
         , Register New (Ref ExprLink') m
         , Event (New (Ref Expr')) m (Universal ref)
         , HasLayerM m Expr' Data

         , AsgMonad n
         , m ~ Layouted ANT n
         , MonadIO n
         , StackCons (Layers ExprLink' (Cfg m)) (Runner m)
         , Self.MonadSelfBuilder (Ref Expr') n
         , Show (Asg (Cfg m) (UntyppedExpr Star ()))
         )
        => n ref
test_gr2 =  layouted @ANT $ do
    (s1 :: Ref (UntyppedExpr Star            ())) <- star
    (s2 :: Ref (UntyppedExpr Star            ())) <- star
    (u1 :: Ref (UntyppedExpr (Unify :> Star) ())) <- unify s1 s2
    u2 <- unify s1 u1
    u3 <- unify s2 u1
    u4 <- unify u2 u3

    t <- read s1
    s1' <- Asg.mark' t
    d <- select @Data t
    print s1'

    case' s1' of
        Unify l r -> print "ppp"
        Star      -> case' s1' of
            Unify l r -> print "hello"
            Star      -> print "hello3xx"
        _         -> print "not found"
    --
    -- let { case_expr = s1'
    -- ;f1 = matchx case_expr $ \(ExprSymbol (Symbol.Unify l r)) -> print ala where
    --     ala = 11
    -- ;f2 = matchx case_expr $ \(ExprSymbol Symbol.Star)        -> (print "hello3" )
    -- } in $(testTH2 'case_expr [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])

    -- print s1'
    return s1
    -- (l,r) <- get @Data l
-- markAsg :: AsgMonad m => a -> m (AsgM m a)

type Select   p m a =  Get     p (Asg.Marked m a)
type Selector p m a = (Getter  p (Asg.Marked m a), Markable m a)
type Updater  p m a = (Setter' p (Asg.Marked m a), Markable m a, AsgMonad m)

select :: forall p m a. Selector p m a => a -> m (Select p m a)
select = get @p <∘> Asg.mark ; {-# INLINE select #-}

update :: forall p m a. Updater p m a => Select p m a -> a -> m (AsgVal a)
update v a = (liftAsg . set' @p v) =<< Asg.mark a ; {-# INLINE update #-}

-- class Setter' t a where set' ::           Get t a -> a -> a

-- select :: forall p m a. (Selector2 p m (Asg.Marked m a), Markable m a) => a -> m (Select2 p m (Asg.Marked m a))
-- select = select2 @p <=< Asg.mark ; {-# INLINE select #-}

test_g4 :: (MonadIO m, PrimMonad m, MonadFix m)
        => m (Asg Net (Ref (UntyppedExpr Star ())), Network3)
test_g4 = runNewGraphT
        $ runAsgT
        $ Event.suppressAll
        $ flip Self.evalT undefined
        $ flip Type.evalT Nothing
        $ runInferenceT2 @TermType @Net
        $ flip (D.evalT UID) (0 :: Int64)
        $ listenAny @(Handler Data ) (handle @Data )
        $ listenAny @(Handler UID  ) (handle @UID  )
        $ listenAny @(Handler Type ) (handle @Type )
        $ listenAny @(Handler Succs) (handle @Succs)
        $ test_gr2





instance Generalize (Compound SimpleX lst) Draft

main :: IO ()
main = do

    (exprRef,g) <- test_g4
    print exprRef


    -- (s1,g) <- test_g3
    -- print s1
    --
    -- let x   = read s1 g
    --     res = elems g
    --     es  = map (flip read g) res
    --
    -- let nodes = ByteString.unpack $ encode $ map visNode es
    --
    -- (_, vis) <- Vis.newRunDiffT $ do
    --     let vss = map (visNode2 g) es
    --         vns = fst <$> vss
    --         ves = join $ snd <$> vss
    --     Vis.addNodes vns
    --     Vis.addEdges ves
    --
    --
    -- let cfg = ByteString.unpack $ encode $ vis
    -- openBrowser ("http://localhost:8200?cfg=" <> cfg)
    --
    --
    -- let tpRef  = get @Type x
    --     tpLink = read tpRef g
    --
    -- print "***"
    -- print $ get @Sym $ get @Data x

    return ()

-- visNode :: HasLayers Elemx t '[Data, UID] => Expr t layout -> Vis.Node
-- visNode el = Vis.Node header (get @UID el) 0 (fromList [header]) where
--     header = fromString $ reprStyled HeaderOnly el
--
-- visNode2 :: forall t g layout b.
--             ( HasLayers Elemx t '[UID, Type]
--             , HasLayers ExprLink' t '[UID]
--             , ASTFunc t g
--
--             , FieldsC t layout -- FIXME[WD]: scratch implementation
--
--             ) => g -> Expr t layout -> (Vis.Node, [Vis.Edge])
-- visNode2 g expr = (node, edges) where
--     node   = Vis.Node (fromString "") (get @UID expr) (get @UID expr) (fromList [header])
--     header = fromString $ reprStyled HeaderOnly expr
--     tpRef  = get @Type expr
--     tpLink = read tpRef g
--     tpUid  = get @UID  tpLink
--     (l,r)  = get @Data tpLink
--     ins    = symbolFields2 expr
--
--     ln     = read l g
--     rn     = read r g
--     --
--     lnUID = get @UID ln
--     rnUID = get @UID rn
--
--     uss = map getUIDs ins
--
--     getUIDs re = (i, lnUID, rnUID) where
--         e      = read re g
--         i      = get @UID  e
--         (l, r) = get @Data e
--         ln     = read l g
--         rn     = read r g
--         lnUID  = get @UID ln
--         rnUID  = get @UID rn
--
--     mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty
--
--     tpVis  = if lnUID == rnUID then [] else [Vis.Edge (fromString "") tpUid tpUid lnUID rnUID (fromList [fromString "type"])]
--     edges  = tpVis <> (mkEdge <$> uss)









-- instance {-# INCOHERENT #-} Constructor a m c => Constructor a (KnownTypeT cls t m) c where
--     cons = lift . cons ; {-# INLINE cons #-}
--
-- instance {-# INCOHERENT #-} (Referable r m, NoOutput m) => Referable r (KnownTypeT cls t m) where
--     refDesc   = lift .  refDesc   @r ; {-# INLINE refDesc   #-}
--     unrefDesc = lift .  unrefDesc @r ; {-# INLINE unrefDesc #-}
--     readDesc  = lift .  readDesc  @r ; {-# INLINE readDesc  #-}
--     writeDesc = lift .: writeDesc @r ; {-# INLINE writeDesc #-}
--
-- instance {-# INCOHERENT #-} StackCons ls m => StackCons ls (KnownTypeT cls t m) where
--     consStack = lift . consStack ; {-# INLINE consStack #-}
--
-- instance {-# INCOHERENT #-} (TTT a m, NoOutput m) => TTT a (KnownTypeT cls t m) where
--     elems'  = lift elems'  ; {-# INLINE elems'  #-}
--     links'  = lift links'  ; {-# INLINE links'  #-}



-- instance {-# INCOHERENT #-} Constructor a m c => Constructor a (Runner m) c where
--     cons = lift . cons ; {-# INLINE cons #-}
--
-- instance {-# INCOHERENT #-} (Referable r m, NoOutput m) => Referable r (Runner m) where
--     refDesc   = lift .  refDesc   @r ; {-# INLINE refDesc   #-}
--     unrefDesc = lift .  unrefDesc @r ; {-# INLINE unrefDesc #-}
--     readDesc  = lift .  readDesc  @r ; {-# INLINE readDesc  #-}
--     writeDesc = lift .: writeDesc @r ; {-# INLINE writeDesc #-}
--
-- instance {-# INCOHERENT #-} StackCons ls m => StackCons ls (Runner m) where
--     consStack = lift . consStack ; {-# INLINE consStack #-}
--
-- instance {-# INCOHERENT #-} (TTT a m, NoOutput m) => TTT a (Runner m) where
--     elems'  = lift elems'  ; {-# INLINE elems'  #-}
--     links'  = lift links'  ; {-# INLINE links'  #-}
