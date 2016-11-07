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


import Data.Graph          hiding (Dynamic, Connection, Ref, Referable, Link, link, Succs, CONNECTION)
import Data.Graph.Builders hiding (Linkable)
import Prologue            hiding (elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter)

import           Control.Monad.Event2     as Event
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
import           Luna.Pretty.GraphViz2
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
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Control.Monad.State.Dependent as D
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Web.Browser (openBrowser)

import qualified Luna.Pretty.Graph as Vis

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



data InfLayers = InfLayers



runCase :: Getter Data (Expr t layout) => Expr t layout -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    d   = get @Data el
    s   = unwrap' $ get @Sym  d
    idx = unwrap' $ get @Atom d
{-# INLINE runCase #-}


matchx :: expr ~ Expr t layout => expr -> (ExprSymbol atom expr -> b) -> x -> b
matchx t f = rebind (f . uniExprTypes t) where
    rebind :: (a -> b) -> x -> b
    rebind f = f . unsafeCoerce ; {-# INLINE rebind #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}





-- === UID layer === --

data UID = UID deriving (Show)

type instance LayerData UID t = Int64

-- FIXME [WD]: Remove when moving out from constructors
instance (D.MonadState UID (LayerData UID expr) m, Monad m)
      => Constructor a m (Layer expr UID) where
    cons _ = Layer <$> D.modify UID (\s -> (succ s, s))

instance (Monad m, D.MonadState UID (LayerData UID expr) m) => LayerCons UID m where
    consLayer _ = Layer <$> D.modify UID (\s -> (succ s, s))


-- === Data layer === --

instance Monad m => LayerCons Data m where
    consLayer = return . Layer ; {-# INLINE consLayer #-}




-- === Succs layer === --

data Succs = Succs deriving (Show)

type instance LayerData Succs t = [Universal t]

instance Monad m => LayerCons Succs m where
    consLayer _ = return def ; {-# INLINE consLayer #-}

instance Monad m => Constructor a m (Layer expr Succs) where
    cons _ = Layer <$> return def ; {-# INLINE cons #-}

instance ( ASTMonadBase t m
         , MonadIO m
         , HasLayer EXPR t UID
         , Show (Expr t Draft)
         , a ~ Ref (Link (Expr t Draft) (Expr t Draft)))
      => Handler Succs (New CONNECTION) a m where
    handle _ rl = do
        l <- read rl
        -- let succs = get @Succs l
        -- return ()
        let (srcRef, tgtRef) = get @Data l
        src <- read srcRef
        tgt <- read tgtRef
        putStrLn $ show (get @UID src) <> " -> " <> show (get @UID tgt)

        return ()

        -- print b
        -- write rl (set @Succs )
        print "there is new CONNECTION!"




type NetLayers   = '[Data, UID, Type, Succs]
type Network3    = NEC.HGraph                '[Node, Edge, Cluster]
type MNetwork3 m = NEC.HMGraph  '[Node, Edge, Cluster] (PrimState m)
type MNetworkX   = NEC.HMGraph  '[Node, Edge, Cluster]






data SimpleX




type family Specialized t spec layout



-- newtype NodeRef     tgt = NodeRef (Ref2 Node tgt) deriving (Show)
-- newtype EdgeRef src tgt = EdgeRef (Ref2 Edge (Arc2 (Ref src) (Ref tgt))) deriving (Show)
--
-- makeWrapped ''NodeRef
-- makeWrapped ''EdgeRef



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



-- FIXME[WD]: refactor ugly Arc3 implementation
-- === Arc3 === --
data Arc3 src tgt = Arc3 Int64 (Arc2 (Ref src) (Ref tgt))

-- type instance Get p (Link (Expr t a) (Expr t b)) = LayerData p t
-- instance HasLinkLayer p t => Getter p (Link (Expr t l) (Expr t r)) where
--     get = linkLayer @p;
--
-- class HasLinkLayer l t where
--     linkLayer :: forall a b. Link (Expr t a) (Expr t b) -> LayerData l t

-- instance HasLinkLayer UID Net where
--     linkLayer (Link (Arc3 uid _)) = uid ; {-# INLINE linkLayer #-}


deriving instance Show (Arc2 (Ref src) (Ref tgt)) => Show (Arc3 src tgt)
-- makeWrapped ''Arc3



-----------------
-- === Net === --
-----------------

-- === Definition === --

data Net = Net
type instance Layers EXPR Net = NetLayers
type instance Layers LINK Net = '[Data, UID]

type instance Impl Ref  (Elem Net)                   = Ref2 Node
type instance Impl Ref  (Link (Elem Net) (Elem Net)) = Ref2 Edge
-- type instance Impl Link (Elem Net)                   = Arc3


-- === Instances === --

-- Refs

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Node g m, ReferableM Node g m, NoOutput m) -- Generator m (Impl Ref (Cfg a) a))
      => Referable (Elem Net) m where
    -- newRef' = Ref <$> new
    ref'   = valOnly . Ref <∘> construct'  ; {-# INLINE ref'   #-}
    read'  = readRef . unwrap'             ; {-# INLINE read'  #-}
    write' = noVal <∘∘> writeRef . unwrap' ; {-# INLINE write' #-}

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, DynamicM3 Edge g m, ReferableM Edge g m, NoOutput m)
      => Referable (Link (Elem Net) (Elem Net)) m where
    ref'   = valOnly . Ref <∘> construct'  ; {-# INLINE ref'   #-}
    read'  = readRef . unwrap'             ; {-# INLINE read'  #-}
    write' = noVal <∘∘> writeRef . unwrap' ; {-# INLINE write' #-}


instance Referable (Elem Net) ((->) Network3) where
    ref'  a t = fooe $ runGraph  (refM  a) t ; {-# INLINE ref'  #-}
    read' a t = evalGraphInplace (readM a) t ; {-# INLINE read' #-}

instance Referable (Link (Elem Net) (Elem Net)) ((->) Network3) where
    ref'  a t = fooe $ runGraph  (refM  a) t ; {-# INLINE ref'  #-}
    read' a t = evalGraphInplace (readM a) t ; {-# INLINE read' #-}


-- Links

-- instance (Monad m, D.MonadState UID Int64 m) => Linkable (Elem Net) m where
--     linkM' l r  = do
--         uid <- D.modify UID (\s -> (succ s, s))
--         (return . Link . Arc3 uid) $ Arc2 l r
--     {-# INLINE linkM' #-}
--     unlinkM' (Link (Arc3 _ (Arc2 l r))) = return (l,r) ; {-# INLINE unlinkM' #-}







valOnly v = (Just' v, Nothing')
noVal   _ = (Nothing', Nothing')

fooe (x,y) = (Just' x, Just' y)
-- class Monad m => Referable t m where
--     ref'   :: forall a. (t ~ Cfg a) =>     a -> Result3 m (Ref a)
--     unref' :: forall a. (t ~ Cfg a) => Ref a -> m ()
--     read'  :: forall a. (t ~ Cfg a) => Ref a -> m a
--     write' :: forall a. (t ~ Cfg a) => Ref a -> a -> m ()
--

-- instance Referable2 (Expr Net layout) ((->) Network3) where
--     ref a = runGraph $ refM a




-- instance (Monad m, MonadBuilder g m, DynamicM3 Edge g m)
    -- linkM' a b = EdgeRef <$> construct' (Arc2 a b)






--
-- class ReferableM r t m where
--     setRefM  :: Ref2 r a -> a -> t -> m t
--     viewRefM :: Ref2 r a      -> t -> m a





-- === Type layer === --

type instance LayerData Type t = SubLink Type t


instance ( expr ~ AnyExpr t
         , ref  ~ Ref expr
         , Linkable t m
         , Referable (Link (Elem t) (Elem t)) m

         , Self.MonadSelfBuilder (Ref (Expr t Draft)) m
         , Type.MonadTypeBuilder ref m
         , Constructor TermStore m (AnyExprStack t), Referable (Elem t) m, MonadFix m
         , RegisterNew2 CONNECTION (Ref (Link (Expr t Draft) (Expr t Draft))) m
         ) => Constructor TermStore m (Layer (AnyExpr t) Type) where -- we cannot simplify the type in instance head because of GHC bug https://ghc.haskell.org/trac/ghc/ticket/12734
    cons _ = do
        self <- anyLayout3 <$> Self.get
        top  <- localTop
        l    <- connect top self
        return $ Layer l






-- | Run a function and use the result as a context type
mfixType :: (Type.MonadTypeBuilder a m, MonadFix m) => m a -> m a
mfixType f = mfix $ flip Type.with' f . Just


localTop :: ( Type.MonadTypeBuilder (Ref (AnyExpr t)) m, Constructor TermStore m (AnyExprStack t)
            , Self.MonadSelfBuilder (Ref (Expr t Draft)) m, MonadFix m, Referable (Elem t) m)
         => m (Ref (AnyExpr t))
localTop = Type.get >>= fromMaybeM (mfixType magicStar)




type AnyExprCons t m = ( Constructor TermStore m (AnyExprStack t)
                       )


type ASTAccess t m = ( Referable (Elem t) m
                     , Referable (Link (Elem t) (Elem t)) m
                     , HasLayer EXPR t Data
                     , HasLayer LINK t Data
                     , Show (AnyExpr t)
                     , Show (Ref (AnyExpr t))
                     )

type ASTModify t m = ( Linkable    t m
                     , AnyExprCons t m
                     )

type ASTBuilder t m = ( Self.MonadSelfBuilder (Ref (Expr t Draft)) m
                      , Inferable2 TermType t m
                      , MonadFix              m
                      , NoOutput              m
                      )

type ASTRegister t m = ( RegisterNew  EXPR       (Ref (Expr t Draft)) m
                       , RegisterNew2 CONNECTION (Ref (Link (Expr t Draft) (Expr t Draft))) m
                       )


type ASTAccessor t ast = ASTAccess t ((->) ast)

type ASTMonadBase t m = ( ASTAccess  t m
                        , ASTModify  t m
                        , ASTBuilder t m
                        , TTT t m -- move to AST access
                        , Constructor TermStore m (LayerStackBase (AnyExpr t) (Layers' (AnyExpr t))) -- FIXME[WD]: this should not be needed. There is somewhere a bug with this context
                        )

type ASTMonad t m = ( ASTMonadBase t m
                    , ASTRegister  t m
                    )

type ASTMonadIO t m = ( ASTMonad t m, MonadIO m )

type ASTMonad' t layout m = ( ASTMonad t m
                              , Inferable2 Layout layout m
                              )

magicStar :: ( AnyExprCons t m, Referable (Elem t) m
             , Self.MonadSelfBuilder (Ref (Expr t Draft)) m)
          => m (Ref (AnyExpr t))
magicStar = Self.put . universal =<<& (expr N.star' >>= refM)


type l <+> r = Merge l r
type l :>> r = Specialized Atom l r


data CONNECTION

data New a

type Register    e a m = (Event e m (Universal a), Self.MonadSelfBuilder (Universal a) m)
type RegisterNew e a m = Register (New e) a m

type Register2    e a m = (Event e m (Universal a))
type RegisterNew2 e a m = Register2 (New e) a m

register :: forall e a m. Register e a m => a -> m ()
register = Self.put <=< dispatch @e ∘ universal ; {-# INLINE register #-}

registerNew :: forall e a m. RegisterNew e a m => a -> m ()
registerNew = register @(New e) ; {-# INLINE registerNew #-}

registerExpr :: RegisterNew EXPR a m => a -> m ()
registerExpr = registerNew @EXPR

-- connect :: (Linkable' src tgt m, Referable' (Link src tgt) m) => Ref src -> Ref tgt -> m (Ref (Link src tgt))
-- connect a b = refM =<< link a b ; {-# INLINE connect #-}

connect :: (Linkable' src tgt m, Referable' (Link src tgt) m, RegisterNew2 CONNECTION (Ref (Link src tgt)) m) => Ref src -> Ref tgt -> m (Ref (Link src tgt))
connect a b = mdo
    r <- refM =<< link a b
    -- dispatch @(New CONNECTION) (universal r)
    return r
{-# INLINE connect #-}




star :: ASTMonad' t layout m => m (Ref (Expr t (Set Atom Star layout)))
star = registerExpr =<<& (mkExpr (wrap' N.star'))

unify :: ASTMonad t m => Ref (Expr t l1) -> Ref (Expr t l2) -> m (Ref (Expr t (Unify :>> (l1 <+> l2))))
unify a b = Self.put . universal =<<& mdo
    n  <- mkExpr (wrap' $ N.unify' la lb)
    la <- connect (unsafeGeneralize a) n
    lb <- connect (unsafeGeneralize b) n
    dispatch @(New CONNECTION) (universal la)
    dispatch @(New CONNECTION) (universal lb)
    return n


instance {-# INCOHERENT #-} Constructor a m c => Constructor a (KnownTypeT cls t m) c where
    cons = lift . cons ; {-# INLINE cons #-}

instance {-# INCOHERENT #-} (Referable r m, NoOutput m) => Referable r (KnownTypeT cls t m) where
    newRef' = lift $ newRef' @r

    ref'   = lift .  ref'   @r ; {-# INLINE ref'   #-}
    unref' = lift .  unref' @r ; {-# INLINE unref' #-}
    read'  = lift .  read'  @r ; {-# INLINE read'  #-}
    write' = lift .: write' @r ; {-# INLINE write' #-}

instance {-# INCOHERENT #-} StackCons ls m => StackCons ls (KnownTypeT cls t m) where
    consStack = lift . consStack ; {-# INLINE consStack #-}

instance {-# INCOHERENT #-} (TTT a m, NoOutput m) => TTT a (KnownTypeT cls t m) where
    elems'  = lift elems'  ; {-# INLINE elems'  #-}
    links'  = lift links'  ; {-# INLINE links'  #-}
    -- groups' = lift groups' ; {-# INLINE groups' #-}



type family   UnsafeGeneralizable a b :: Constraint
type instance UnsafeGeneralizable (Ref a) (Ref b) = UnsafeGeneralizable a b
type instance UnsafeGeneralizable (Expr t l1) (Expr t l2) = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




nmagicStar :: (AnyExprCons t m, Referable (Elem t) m) => m $ Ref (Expr t layout)
nmagicStar = mkExpr (wrap' N.star')


type ANT' a n t = ANTLayout SimpleX a n t

type Expr' cls a n t = Expr cls (ANT' a n t)
type UntyppedExpr cls a n = Expr' cls a n Star

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

test_g3 :: (MonadIO m, PrimMonad m, MonadFix m)
        => m (Ref (UntyppedExpr Net Star ()), Network3)
test_g3 = runNewGraphT
        $ Event.suppressAll
        $ flip Self.evalT undefined
        $ flip Type.evalT Nothing
        $ runInferenceT2 @TermType @Net
        $ flip (D.evalT UID) (0 :: Int64)
        $ listenAny @(Handler Data ) (handle @Data )
        $ listenAny @(Handler UID  ) (handle @UID  )
        $ listenAny @(Handler Type ) (handle @Type )
        $ listenAny @(Handler Succs) (handle @Succs)
        $ test_gr


class Monad m => Handler layer event a m where handle :: Proxy event -> a -> m ()

instance {-# OVERLAPPABLE #-} Monad m => Handler layer event a m where handle _ _ = return () ; {-# INLINE handle #-}





instance {-# OVERLAPPABLE #-} (Monad m, MonadBuilder g m, ReferableM Node g m, Inferable2 TermType Net m, NoOutput m) => TTT Net m where
    elems' = (Ref . unsafeRefer) <<∘>> viewPtrs =<< GraphBuilder.get

instance Inferable2 TermType Net ((->) Network3)
instance TTT Net ((->) Network3) where
    elems' = evalGraphInplace $ runInferenceT2 @TermType @Net elemsM ; {-# INLINE elems' #-}



--
-- instance Referable (Elem Net) ((->) Network3) where
--     ref'  a t = fooe $ runGraph  (refM  a) t ; {-# INLINE ref'  #-}
--     read' a t = evalGraphInplace (readM a) t ; {-# INLINE read' #-}

--
-- Constructor TermStore m (AnyExprStack t)
--
-- type ExprStack    t layout = LayerStack (Expr t layout)
-- type AnyExprStack t        = ExprStack t Layout.Any
--

-- type    LayerStackBase t = Stack (Layer t)
-- newtype LayerStack     t = LayerStack (LayerStackBase t (Layers' t))


test_gr :: forall t m . ASTMonadIO t m => m (Ref (UntyppedExpr t Star ()))
test_gr = layouted @ANT $ do
    (s1 :: Ref (UntyppedExpr t Star            ())) <- star
    (s2 :: Ref (UntyppedExpr t Star            ())) <- star
    (u1 :: Ref (UntyppedExpr t (Unify :> Star) ())) <- unify s1 s2
    u2 <- unify s1 u1
    u3 <- unify s2 u1
    u4 <- unify u2 u3

    t <- read s1
    write s1 t

    let u1'  = generalize u1 :: Ref (Expr t Draft)

    -- let u1'  = generalize u1 :: Ref (UntyppedExpr t layers Draft ())
    -- let u1'  = generalize u1 :: Link    (UntyppedExpr t layers Draft ())

    -- let u1'  = generalize u1 :: Ref Node (UntyppedExpr t layers Draft ())
    -- let u1'  = generalize u1 :: Ref Edge (UntyppedExpr t layers Draft ())
        -- u1'' =

    -- bs <- bindings
    -- Unify l r <- read u1
    -- tgt  <- targetM l
    -- src  <- sourceM l
    -- both <- read l
    es <- elems
    es' <- mapM read es

    case' t of
        Unify l r -> print "ppp"
        Star      -> case' t of
            Unify l r -> print "hello"
            Star      -> print "hello3xx"
        _         -> print "not found"

    --
    -- let { case_expr = t
    -- ;f1 = matchx case_expr $ \(ExprSymbol (Symbol.Unify l r)) -> print ala where
    --     ala = 11
    -- ;f2 = matchx case_expr $ \(ExprSymbol Symbol.Star)       -> (print "hello3" )
    --    where ala = 11
    -- } in $(testTH2 'case_expr [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])
    --
    -- -- es <- elements2
    --
    --
    print "!!!"
    print t
    print es
    print es'
    print s1
    return s1



-- vis

instance Generalize (Compound SimpleX lst) Draft

main :: IO ()
main = do

    -- let (s,g) = runST test_g3
    (s1,g) <- test_g3

    let x   = read s1 g
        x'  = generalize x :: Expr Net Draft
        res = elems g
        es  = map (flip read g) res


        --
        -- putStrLn "\n\n---"
        --
        -- let t  = read  s1 g
        --     g' = write s1 t g
        --     bs = bindings2 g
        --     os = elements2 g
        --
        -- putStrLn $ reprStyled HeaderOnly t
        -- print t
        -- -- print os
    let nodes = ByteString.unpack $ encode $ map visNode es

    -- putStrLn ">>>>\n"
    -- print nodes

    (_, vis) <- Vis.newRunDiffT $ do
        let vss = map (visNode2 g) es
            vns = fst <$> vss
            ves = join $ snd <$> vss
        Vis.addNodes vns
        Vis.addEdges ves
        -- print "!!!!"
        -- print ves


    let cfg = ByteString.unpack $ encode $ vis
    -- putStrLn cfg
    openBrowser ("http://localhost:8200?cfg=" <> cfg)


    -- print "----"
    -- print x
    let tpRef  = get @Type x
        tpLink = read tpRef g

    print "***"
    print $ get @Sym $ get @Data x
    print $ symbolFields2 x'
    -- print $ get @UID tpLink

    return ()

visNode :: HasLayers EXPR t '[Data, UID] => Expr t layout -> Vis.Node
visNode el = Vis.Node header (get @UID el) 0 (fromList [header]) where
    header = fromString $ reprStyled HeaderOnly el

visNode2 :: forall t g layout b.
            ( HasLayers EXPR t '[Data, UID, Type]
            , HasLayers LINK t '[Data, UID]
            , ASTAccessor t g

            , FieldsC t layout

            ) => g -> Expr t layout -> (Vis.Node, [Vis.Edge])
visNode2 g expr = (node, edges) where
    node   = Vis.Node (fromString "") (get @UID expr) (get @UID expr) (fromList [header])
    header = fromString $ reprStyled HeaderOnly expr
    tpRef  = get @Type expr
    tpLink = read tpRef g
    tpUid  = get @UID  tpLink
    (l,r)  = get @Data tpLink
    ins    = symbolFields2 expr

    ln     = read l g
    rn     = read r g
    --
    lnUID = get @UID ln
    rnUID = get @UID rn

    uss = map getUIDs ins

    getUIDs re = (i, lnUID, rnUID) where
        e      = read re g
        i      = get @UID  e
        (l, r) = get @Data e
        ln     = read l g
        rn     = read r g
        lnUID  = get @UID ln
        rnUID  = get @UID rn

    mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty

    tpVis  = if lnUID == rnUID then [] else [Vis.Edge (fromString "") tpUid tpUid lnUID rnUID (fromList [fromString "type"])]
    edges  = tpVis <> (mkEdge <$> uss)
