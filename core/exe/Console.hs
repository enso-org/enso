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
{-# LANGUAGE UndecidableSuperClasses   #-} -- Only for symbol mapping, to be refactored
{-# LANGUAGE NoOverloadedStrings       #-} -- https://ghc.haskell.org/trac/ghc/ticket/12797

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Prologue            hiding (typeRep, elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter, set, Setter', set')
import qualified Prologue as P

import           Control.Monad.Event     as Event
import qualified Control.Monad.Delayed    as Delayed
import           Control.Monad.Delayed    (Runner, MonadDelayed, Delayed, delayed)
import qualified Control.Monad.Writer     as Writer
import           Data.Construction        hiding (Register, register)
import           Data.Container           (index_)
import           Data.Container           hiding (impossible, elems, elems', elemsM)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
-- import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import           Data.Container.Hetero                           (Hetero(..), Any(..))
import           Luna.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Pass.Inference.Importing       as Importing
import           Luna.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Env.Env                                 as Env
import qualified Luna.IR.Library.Symbol                             as Symbol
import           Luna.IR.Term.Format


import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library



-- import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.IR.Term hiding (Data, cons, unify, star, Readable)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.IR.Internal.IR as IR
import Luna.IR.Internal.IR hiding (Bind, Fields, (:=))

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
import Luna.Pass.Class (Keys, Preserves, Pass, Elements, Inputs, Outputs)
import Luna.IR.Layer.UID (UID, ID)
import qualified Luna.IR.Layer.UID as UID
import Luna.IR.Layer.Succs
import Luna.IR.Layer.Type
import qualified Data.ManagedVectorMap as MV
import Type.Maybe (FromJust)

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers





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






type l <+> r = Merge l r
type l :>> r = Specialized Atom l r




type AtomicTerm atom layout = Term (Update Atom atom layout)



star :: (IRMonad m, Inferable2 Layout layout m) => m (AtomicTerm Star layout)
star = term Sym.uncheckedStar
{-# INLINE star #-}

unify :: IRMonad m => Term l -> Term l' -> m (Term (Unify :>> (l <+> l')))
unify a b = mdo
    n  <- term $ Sym.uncheckedUnify la lb
    la <- link (unsafeGeneralize a) n
    lb <- link (unsafeGeneralize b) n
    return n
{-# INLINE unify #-}



type family   UnsafeGeneralizable a b :: Constraint

-- type instance UnsafeGeneralizable (Ref a) (Ref b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Term l) (Term l') = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}

type ANT' a n t = ANTLayout SimpleX a n t



type AntTerm a n t = Term (ANT' a n t)
type UntyppedTerm a n = AntTerm a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
layouted = baseLayout @(DefaultLayout l)



instance Generalize (Compound SimpleX lst) Draft




-- FIXME: moze po prostu Term Star?
newtype MagicStar = MagicStar Term'

makeWrapped ''MagicStar

newMagicStar :: IRMonad m => m MagicStar
newMagicStar = wrap' <$> term Sym.uncheckedStar ; {-# INLINE newMagicStar #-}

magicStar :: Iso' (Term l) MagicStar
magicStar = iso (wrap' . unsafeCoerce) (unsafeCoerce . unwrap') ; {-# INLINE magicStar #-}


type instance KeyTargetST m (Attr MagicStar) = Maybe MagicStar



data                    SimpleAA
type instance Elements  SimpleAA = '[TERM, Link' TERM]
type instance Inputs    SimpleAA = '[Layer TERM Model, Layer TERM UID, Layer TERM Type]
type instance Outputs   SimpleAA = '[Layer TERM Model, Layer TERM UID, Layer TERM Type]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m) => m (Either Pass.Err ())
test_pass1 = runIRT $ do
    runRegs

    attachLayer (typeRep @Model) (typeRep @TERM)
    attachLayer (typeRep @Succs) (typeRep @TERM)
    attachLayer (typeRep @Type)  (typeRep @TERM)
    attachLayer (typeRep @UID)   (typeRep @TERM)

    Pass.eval pass1


gen_pass1 :: (MonadIO m, IRMonad m, Readable (Layer TERM Model) m, Readable (Layer TERM Type) m) => m ()
gen_pass1 = layouted @ANT $ do
    (s1 :: UntyppedTerm Star ()) <- star
    (s2 :: UntyppedTerm Star ()) <- star
    u1 <- unify s1 s2
    print "hello"
    d <- readLayer @Type u1
    print d

    match s1 $ \case
        Unify l r -> print "ppp"
        Star      -> match s1 $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"

    return ()



consTypeLayer :: IRMonad m
              => MV.STRefM m (Maybe MagicStar) -> Term t -> Definition (Term t) -> m (LayerData Type (Term t))
consTypeLayer ref self _ = do
    top  <- view (from magicStar) <$> localTop ref
    conn <- link top self
    return conn


localTop :: IRMonad m
         => MV.STRefM m (Maybe MagicStar) -> m MagicStar
localTop ref = MV.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> mdo
        MV.writeSTRef ref $ Just s
        s <- newMagicStar
        MV.writeSTRef ref Nothing
        return s


layerReg4 :: IRMonad m => m ()
layerReg4 = registerElemLayer @TERM @Type . consTypeLayer =<< runInIR (MV.newSTRef Nothing)




runRegs :: IRMonad m => m ()
runRegs = do
    runElemRegs
    runLayerRegs

-- === Elem reg defs === --

elemRegs :: IRMonad m => [m ()]
elemRegs = [elemReg1, elemReg2]

runElemRegs :: IRMonad m => m ()
runElemRegs = sequence_ elemRegs

elemReg1 :: IRMonad m => m ()
elemReg1 = registerElem @TERM

elemReg2 :: IRMonad m => m ()
elemReg2 = registerElem @(LINK' TERM)


-- === Layer reg defs === --

layerRegs :: IRMonad m => [m ()]
layerRegs = [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: IRMonad m => m ()
runLayerRegs = sequence_ layerRegs


layerReg1 :: IRMonad m => m ()
layerReg1 = registerGenericLayer @Model $ \ _ -> return

layerReg2 :: IRMonad m => m ()
layerReg2 = registerGenericLayer @Succs $ \ _ _ -> return def



consUIDLayer :: PrimMonad m => MV.STRefM m ID -> t -> Definition t -> m (LayerData UID t)
consUIDLayer ref _ _ = MV.modifySTRef' ref (\i -> (i, succ i))

layerReg3 :: IRMonad m => m ()
layerReg3 = registerGenericLayer @UID . consUIDLayer =<< runInIR (MV.newSTRef 0)





main :: IO ()
main = do
    -- test_g4
    p <- test_pass1
    print p
    return ()








-- -- === Succs layer === --
--
-- data Succs = Succs deriving (Show)
--
-- -- FIXME[WD]: refactorme
-- type instance LayerData Succs a = S.Set (XYZ a)
-- type family XYZ a where XYZ (IR t a) = IR t (Ref (Universal a))
--
-- instance Monad m => LayerCons Succs m where
--     consLayer _ = return def ; {-# INLINE consLayer #-}
--
-- instance Monad m => Constructor a m (Layer expr Succs) where
--     cons _ = Layer <$> return def ; {-# INLINE cons #-}
--
--
-- instance ( IRMonad m
--          , IRReferables m
--          , HasLayerM  m ExprLink' Model
--          , HasLayersM m Expr' '[UID, Succs]
--          , Ord (IRM m (Ref (Expr Draft)))
--          , Eq  (IRM m (Ref (Expr Draft)))
--          )
--       => Handler Succs New (Ref ExprLink') m where
--     handle linkRef = do
--         (srcRef, tgtRef) <- select @Model =<< readx linkRef
--         modifyx srcRef $ with @Succs (S.insert tgtRef)

--
--
-- type Network3    = NEC.HGraph                '[Node, Edge, Cluster]
-- type MNetwork3 m = NEC.HMGraph  '[Node, Edge, Cluster] (PrimState m)
-- type MNetworkX   = NEC.HMGraph  '[Node, Edge, Cluster]





        --
        -- -- === Events handler === --
        --
        -- class    Monad m                     => ProxyHandler layer event a m where proxyHandle :: Proxy event -> a -> m ()
        -- instance ReHandle layer event op a m => ProxyHandler layer event a m where proxyHandle _ = handle @layer @op ; {-# INLINE proxyHandle #-}
        -- type     ReHandle layer event op a m = (event ~ op a, Handler layer op a m, Monad m)
        --
        -- class                         Monad m => Handler layer (op :: * -> *) a m where handle :: a -> m ()
        -- instance {-# OVERLAPPABLE #-} Monad m => Handler layer op             a m where handle _ = return () ; {-# INLINE handle #-}
        --
        --
        -- -- === Atomatic listeners discovery === --
        --
        -- type family Listeners ls m where
        --     Listeners '[]       m = m
        --     Listeners (l ': ls) m = AnyListener (ProxyHandler l) (Listeners ls m)
        --
        -- instance (Monad (Listeners ls m), EventsHandler ls m)
        --       => EventsHandler (l ': ls) m where handleEvents = handleEvents @ls . listenAny @(ProxyHandler l) (proxyHandle @l) ; {-# INLINE handleEvents #-}
        -- instance EventsHandler '[]       m where handleEvents = id ; {-# INLINE handleEvents #-}
        -- class    EventsHandler ls        m where handleEvents :: Listeners ls m a -> m a
        --
        -- type LayerListeners     m = Listeners     (AllLayers (Cfg m)) m
        -- type LayerEventsHandler m = EventsHandler (AllLayers (Cfg m)) m
        --
        -- handleLayerEvents :: forall m a. LayerEventsHandler m => LayerListeners m a -> m a
        -- handleLayerEvents = handleEvents @(AllLayers (Cfg m))



-- test_g4 :: (MonadIO m, PrimMonad m, MonadFix m)
--         => m ()
-- test_g4 = flip (D.evalT UID) (0 :: Int64) $ do
--         (_, vis) <- Vis.newRunDiffT $ do
--             (exprRef, g) <- runNewGraphT $ runNetBuilder test_gr2
--             return ()
--         let cfg = ByteString.unpack $ encode $ vis
--         putStrLn cfg
--         liftIO $ openBrowser ("http://localhost:8200?cfg=" <> cfg)
--         return ()
--
--         -- star :: (ExprBuilder2 m, Inferable2 Layout layout m) => m (AtomicTerm Star layout)






--
-- test_gr2 :: ( ASGBuilder (Layouted ANT m)
--             , HasLayerM  m ExprLink' UID
--             , HasLayersM m Expr'     '[Type, UID]
--             , IRShow m (UntyppedExpr Star ())
--             , MonadVis m
--             , MonadIO m
--
--             , IRMonad m
--             , PrimState (GetIRMonad m) ~ PrimState m -- FIXME
--             , PrimMonad m -- FIXME
--             )
--          => m (Ref (UntyppedExpr Star ()))
-- test_gr2 =  layouted @ANT $ do
--     registerElem  @TERM
--     registerLayer @Model
--     attachLayer   modelRep exprRep
--
--     (sx1 :: UntyppedTerm Star ()) <- star
--     (sx2 :: UntyppedTerm Star ()) <- star
--     -- Just (data_ :: Key m RW TERM Model) <- lookupKey exprRep modelRep
--     Just (data_ :: LayerKey RW TERM Model) <- uncheckedLookupKey
--     print sx1
--     print sx2
--     print =<< readKey data_ sx1
--     (s1 :: Ref (UntyppedExpr Star            ())) <- star
--     snapshot "s1"
--     (s2 :: Ref (UntyppedExpr Star            ())) <- star
--     snapshot "s2"
--     (u1 :: Ref (UntyppedExpr (Unify :> Star) ())) <- unify s1 s2
--     snapshot "s3"
--     u2 <- unify s1 u1
--     snapshot "s4"
--     u3 <- unify s2 u1
--     snapshot "s5"
--     u4 <- unify u2 u3
--     snapshot "s6"
--
--     t <- readx s1
--     s1' <- IR.mark' t
--     -- Just dkey <- askKey @'RW @Expr' @Model
--     print "!!!!!!!!----"
--     -- print (unwrap' dkey)
--     d <- select @Model t
--     print s1'
--
--     matchM t $ \case
--         Unify l r -> print "ppp"
--         Star      -> matchM t $ \case
--             Unify l r -> print "hola"
--             Star      -> print "hellox"
--
--
--     return s1
--
-- -- matchy :: IRMonad m => a -> f -> m (IRM m a)
-- -- matchy t f = IR.mark' t
-- -- matchy t f = IR.mark' t >>= (exprUniSymbol . f)
--
-- matchy :: HasLayer t Expr' Model => (IR t (Expr layout)) -> (Unwrapped (TermUniSymbol (Expr layout)) -> b) -> b
-- matchy a f = f $ unwrap' (exprUniSymbol a)
--
-- matchM :: (HasLayerM m Expr' Model, IRMonad m) => Expr layout -> (Unwrapped (TermUniSymbol (Expr layout)) -> m b) -> m b
-- matchM a f = mark' a >>= flip matchy f
--
--
-- snapshot :: Vis m => P.String -> m()
-- snapshot title = do
--     res <- exprs
--     es  <- mapM readx res
--     vss <- mapM visNode2 es
--     let vns = fst <$> vss
--         ves = join $ snd <$> vss
--     Vis.addStep (fromString title) vns ves
--
--
--
--
-- type Vis m = ( ASGBuilder m
--              , MonadVis m
--              , HasLayersM m Expr'     '[UID, Type]
--              , HasLayerM  m ExprLink' UID
--              )
-- visNode2 :: Vis m => Expr' -> m (Vis.Node, [Vis.Edge])
-- visNode2 expr = do
--     mexpr  <- mark' expr
--     euid   <- select @UID expr
--     tpRef  <- select @Type expr
--     tpLink <- readx tpRef
--     tpUid  <- select @UID  tpLink
--     (l,r)  <- select @Model tpLink
--
--     ln     <- readx l
--     rn     <- readx r
--     -- --
--     lnUID <- select @UID ln
--     rnUID <- select @UID rn
--
--     let header = fromString $ reprStyled HeaderOnly mexpr
--         node   = Vis.Node (fromString "") euid euid (fromList [header])
--         ins    = symbolFields mexpr
--         tpVis  = if lnUID == rnUID then [] else [Vis.Edge (fromString "") tpUid tpUid lnUID rnUID (fromList [fromString "type"])]
--         mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty
--         getUIDs re = do
--             e      <- readx re
--             i      <- select @UID  e
--             (l, r) <- select @Model e
--             ln     <- readx l
--             rn     <- readx r
--             lnUID  <- select @UID ln
--             rnUID  <- select @UID rn
--             return (i, lnUID, rnUID)
--
--     uss <- mapM getUIDs ins
--
--     let edges = tpVis <> (mkEdge <$> uss)
--     return (node, edges)
