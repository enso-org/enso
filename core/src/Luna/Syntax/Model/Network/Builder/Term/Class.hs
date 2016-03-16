{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

{-# LANGUAGE PolyKinds            #-}

module Luna.Syntax.Model.Network.Builder.Term.Class (module Luna.Syntax.Model.Network.Builder.Term.Class, module X) where

import Prelude.Luna hiding (Num)

import           Control.Monad.Event
import           Data.Direction
import           Data.Graph
import           Data.Graph.Builders
import           Data.Layer
import           Data.Layer.Cover
import           Data.Prop
import qualified Data.Record                             as Record
import           Data.Record                             (RecordOf, IsRecord, HasRecord, record, asRecord, Variant, MapTryingElemList_, withElement_, Props, withElement', Layout_Variants, MapTryingElemList, OverElement, overElement)
import           Data.Tuple.Curry.Missing
import           Data.Tuple.OneTuple
import           Luna.Evaluation.Runtime                 as Runtime
import           Luna.Syntax.AST.Function.Argument
import           Luna.Syntax.AST.Function.Argument       as X (arg)
import           Luna.Syntax.AST.Term                    hiding (Val, Lit, Thunk, Expr, Draft, Source, Name)
import qualified Luna.Syntax.AST.Term                    as Term
import qualified Data.Graph.Builder                      as GraphBuilder
import           Luna.Syntax.Model.Layer                 (Type, Markable, TCData, Meta, Name, Lambda, (:<:), (:<))
import           Luna.Compilation.Pass.Interpreter.Layer (InterpreterData)
import           Luna.Syntax.Model.Network.Builder.Layer
import qualified Luna.Syntax.Model.Network.Builder.Self  as Self
import qualified Luna.Syntax.Model.Network.Builder.Type  as Type
import           Luna.Syntax.Model.Network.Term
import qualified Luna.Syntax.AST.Term.Lit                as Lit
import           Control.Monad.Trans.Identity
import           Type.Bool

import Data.Graph.Backend.VectorGraph

import Control.Monad.Delayed (delayed, MonadDelayed)
import Data.Graph.Builder (write)
import qualified Control.Monad.State as State

-------------------------------------
-- === Term building utilities === --
-------------------------------------

-- === Utility Type Families === --

-- FIXME[WD]: Zmienic nazwe Layout na adekwatna
-- FIXME[WD]: Skoro Layout okresla jakie jest "wejscie" do nodu, to Input nie jest potrzebny, bo mozna go wyinferowac jako odwrotnosc Connection

type family BuildArgs (t :: k) n :: *
type family Expanded  (t :: k) n :: *


-- === ElemBuilder === --

class    ElemBuilder el m  a where buildElem :: el -> m a
instance {-# OVERLAPPABLE #-} ElemBuilder el IM a where buildElem = impossible


class    ElemBuilder2 el m  a where buildElem2 :: el -> m a
instance {-# OVERLAPPABLE #-} ElemBuilder2 el IM a where buildElem2 = impossible


instance {-# OVERLAPPABLE #-}
         ( Record.Cons el (Uncovered a)
         , CoverConstructor m a
         , Dispatcher ELEMENT a m
         , Self.MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder el m a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem el = dispatch ELEMENT =<< Self.buildAbsMe (constructCover $ Record.cons el) where
    {-# INLINE buildElem #-}

instance {-# OVERLAPPABLE #-}
         ( CoverConstructor m a
         , Uncovered a ~ el
         , Dispatcher ELEMENT a m
         , Self.MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder2 el (m :: * -> *) a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem2 el = dispatch ELEMENT =<< Self.buildAbsMe (constructCover el) where
    {-# INLINE buildElem2 #-}


-- === TermBuilder === --

class TermBuilder (t :: k) m a where buildTerm :: Proxy t -> BuildArgs t a -> m a
instance {-# OVERLAPPABLE #-} TermBuilder t IM a where buildTerm = impossible




newtype BindBuilder t m a = BindBuilder (IdentityT m a) deriving (Show, Functor, Applicative, Monad, MonadTrans)
makeWrapped ''BindBuilder

runBindBuilder :: BindBuilder t m t -> m t
runBindBuilder = runIdentityT ∘ unwrap'


-------------------------------
-- === Term constructors === --
-------------------------------


-- === Lit === --

type instance BuildArgs Lit.Star   n = ()
type instance BuildArgs Lit.String n = OneTuple Lit.String
type instance BuildArgs Lit.Number n = OneTuple Lit.Number

instance ElemBuilder Lit.Star   m a => TermBuilder Lit.Star   m a where buildTerm p ()           = buildElem Lit.Star
instance ElemBuilder Lit.String m a => TermBuilder Lit.String m a where buildTerm p (OneTuple s) = buildElem s
instance ElemBuilder Lit.Number m a => TermBuilder Lit.Number m a where buildTerm p (OneTuple s) = buildElem s

star :: TermBuilder Lit.Star m a => m a
star = curryN $ buildTerm (Proxy :: Proxy Lit.Star)

str :: TermBuilder Lit.String m a => String -> m a
str = (curryN $ buildTerm (Proxy :: Proxy Lit.String)) ∘ Lit.String

ratio :: TermBuilder Lit.Number m a => Rational -> m a
ratio = (curryN $ buildTerm (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Rational

int :: TermBuilder Lit.Number m a => Integer -> m a
int = (curryN $ buildTerm (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Integer

double :: TermBuilder Lit.Number m a => Double -> m a
double = (curryN $ buildTerm (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Double


-- === Val === --

type instance BuildArgs Cons n = (NameInput n, [Arg (Input n)])
type instance BuildArgs Lam  n = ([Arg (Input n)], Input n)

instance ( name ~ NameInput a
         , inp ~ Input a
         , MonadFix m
         , Connectible     inp  a m
         , ConnectibleName name a m
         , ElemBuilder (Cons (NameConnection name a) (Ref Edge $ Connection inp a)) m a
         ) => TermBuilder Cons m a where
    buildTerm p (name, args) = mdo
        out   <- buildElem $ Cons cname cargs
        cname <- nameConnection name out
        cargs <- (mapM ∘ mapM) (flip connection out) args
        return out

instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder (Lam $ Ref Edge $ Connection inp a) m a
         ) => TermBuilder Lam m a where
    buildTerm p (args, res) = mdo
        out   <- buildElem $ Lam cargs cres
        cargs <- (mapM ∘ mapM) (flip connection out) args
        cres  <- connection res out
        return out


cons :: TermBuilder Cons m a => NameInput a -> [Arg $ Input a] -> m a
cons = curryN $ buildTerm (Proxy :: Proxy Cons)

lam :: TermBuilder Lam m a => [Arg $ Input a] -> Input a -> m a
lam = curryN $ buildTerm (Proxy :: Proxy Lam)


-- === Thunk === --

type instance BuildArgs Acc n    = (NameInput n, Input n)
type instance BuildArgs App n    = (Input n, [Arg (Input n)])
type instance BuildArgs Native n = OneTuple (NameInput n)

instance {-# OVERLAPPABLE #-}
         ( src  ~ Input a
         , name ~ NameInput a
         , MonadFix m
         , Connectible     src  a m
         , ConnectibleName name a m
         , ElemBuilder (Acc (NameConnection name a) (Ref Edge $ Connection src a)) m a
         ) => TermBuilder Acc m a where
    buildTerm p (name, src) = mdo
        out   <- buildElem $ Acc cname csrc
        cname <- nameConnection name out
        csrc  <- connection     src  out
        return out

instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder (App $ Ref Edge $ Connection inp a) m a
         ) => TermBuilder App m a where
    buildTerm p (src, args) = mdo
        out   <- buildElem $ App csrc cargs
        csrc  <- connection src out
        cargs <- (mapM ∘ mapM) (flip connection out) args
        return out

instance ( name ~ NameInput a
         , MonadFix m
         , ConnectibleName name a m
         , ElemBuilder (Native $ NameConnection name a) m a
         ) => TermBuilder Native m a where
    buildTerm p (OneTuple name) = mdo
        out   <- buildElem $ Native cname
        cname <- nameConnection name out
        return out

acc :: TermBuilder Acc m a => NameInput a -> Input a -> m a
acc = curryN $ buildTerm (Proxy :: Proxy Acc)

app :: TermBuilder App m a => Input a -> [Arg $ Input a] -> m a
app = curryN $ buildTerm (Proxy :: Proxy App)

native :: TermBuilder Native m a => NameInput a -> m a
native = curryN $ buildTerm (Proxy :: Proxy Native)

-- === Expr === --


type instance BuildArgs Var n = OneTuple (NameInput n)
instance ( name ~ NameInput a
         , MonadFix m
         , ConnectibleName name a m
         , ElemBuilder (Var $ NameConnection name a) m a
         ) => TermBuilder Var m a where
    buildTerm p (OneTuple name) = mdo
        out   <- buildElem $ Var cname
        cname <- nameConnection name out
        return out

type instance BuildArgs Unify n = (Input n, Input n)
instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder (Unify $ Ref Edge $ Connection inp a) m a
         ) => TermBuilder Unify m a where
    buildTerm p (a,b) = mdo
        out <- buildElem $ Unify ca cb
        ca  <- connection a out
        cb  <- connection b out
        return out

type instance BuildArgs Match n = (Input n, Input n)
instance ( a ~ Input a
         , Record.Cons (Match (Binding a)) (TermOf a)
         , TermBindBuilder n   a
         , Bindable        n   a
         , Binder          n m a
         ) => TermBuilder Match m a where
    buildTerm p (a,b) = evalBindings $ matchCons <$> bind a <*> bind b



        --type instance BuildArgs Unify n = (Input n, Input n)
        --instance ( a ~ Input a
        --         , Record.Cons (Unify (Binding a)) (TermOf a)
        --         , TermBindBuilder n   a
        --         , Bindable        n   a
        --         , Binder          n m a
        --         ) => TermBuilder Unify m a where
        --    buildTerm p (a,b) = evalBindings $ unifyCons <$> bind a <*> bind b


varCons   = Record.cons ∘  Var
unifyCons = Record.cons ∘∘ Unify
matchCons = Record.cons ∘∘ Match


--type TermCons a = SmartCons (Match (Binding a)) (TermOf a)

--class TermBuilder (t :: k) m a where buildTerm :: Proxy t -> BuildArgs t a -> m a

--class

--evalBindings :: (Binder n m a, TermBindBuilder m a)
--             => BindBuilder a (n m) (TermOf a) -> m a
evalBindings m = runBinder $ runBindBuilder $ (lift ∘ buildTerm') =<< m

var :: TermBuilder Var m a => NameInput a -> m a
var = curryN $ buildTerm (Proxy :: Proxy Var)

unify :: TermBuilder Unify m a => Input a -> Input a -> m a
unify = curryN $ buildTerm (Proxy :: Proxy Unify)

match :: TermBuilder Match m a => Input a -> Input a -> m a
match = curryN $ buildTerm (Proxy :: Proxy Match)




type family Binding a
type family NameBinding a
type family Source2 a

--class TermBindBuilder m a where
--    bindName   :: NameInput a -> m (NameBinding a)
--    bind       :: a -> m (Binding a)
--    buildTerm' :: Source2 a -> m a

--class TransRunner n m

--class (MonadTrans n, Monad (n m), Monad m) => Binder n m a | m a -> n where
--    runBinder :: n m a -> m a

class (Monad n, Monad m) => Binder n m a | m a -> n where
    runBinder :: n a -> m a


--newtype Root a   = Root a   deriving (Show, Functor, Foldable, Traversable)
--newtype Slot a   = Slot a   deriving (Show, Functor, Foldable, Traversable)
--data    Bind a b = Bind a b deriving (Show)


--class BindingResolver t m a | t -> a where
--    resolveBinding :: t -> m ()

--instance ( GraphBuilder.MonadBuilder (Hetero (VectorGraph node edge cluster)) m
--         , BindingResolver t m v
--         , State.MonadState [(a, Ref Edge a)] m
--         ) => BindingResolver (Bind t a) m v where
--    resolveBinding (Bind t a) = do
--        cref <- reserveConnection
--        State.modify ((a, cref):)
--        resolveBinding t -- dorobic aplikowanie trzeba







class Bindable m t where
    bindName   :: NameInput t -> BindBuilder t m (NameBinding t)
    bind       :: t           -> BindBuilder t m     (Binding t)


class TermBindBuilder m t where
    buildTerm' :: TermOf t -> m t


--instance TermBindBuilder m t where buildTerm' = undefined

--class NamedConnectionReservation     src tgt m conn where reserveNamedConnection  ::             src -> Proxy tgt -> m conn


instance ( GraphBuilder.MonadBuilder (Hetero (VectorGraph node edge cluster)) m
         , NamedConnectionReservation (NameInput (Ref Node a)) (Ref Node a) m (NameBinding (Ref Node a))
         , State.MonadState [(Ref Node a, Ref Edge (Link a))] m
         ) => Bindable m (Ref Node a) where
    bind t = do
        cref <- reserveConnection
        lift $ State.modify ((t, cref):) -- FIXME[WD]: remove lift
        return cref
    -- FIXME[WD]: add the logic for dynamic graphs to add pending connections to state, like above
    bindName name = lift $ reserveNamedConnection name (p :: P (Ref Node a))
    {-# INLINE bind     #-}
    {-# INLINE bindName #-}


--write :: (MonadBuilder t m, Referred r a t) => Ref r a -> a -> m ()
--write ref = modify_ ∘ set (focus ref)



type instance TermOf (Ref t a) = TermOf a
type instance Binding (Ref Node a) = Ref Edge (Link a)
type instance NameBinding (Ref Node a) = Lit.String -- FIXME[WD]: Support dynamic terms

--Term t Val  Static


--Term t term rt

--class TermBuilder t term rt where
--    bind ::


-- === Draft === --

type instance BuildArgs   Blank n = ()
instance      ElemBuilder Blank m a => TermBuilder Blank m a where buildTerm p () = buildElem Blank

blank :: TermBuilder Blank m a => m a
blank = curryN $ buildTerm (Proxy :: Proxy Blank)






matchType' :: t -> t -> t
matchType' = const





matchType :: Proxy t -> t -> t
matchType _ = id

matchTypeM :: Proxy t -> m t -> m t
matchTypeM _ = id




------------------------------
-- === Network Building === --
------------------------------

type NetClusterLayers = '[Lambda, Name]
type NetLayers        = '[Type, Succs, TCData, InterpreterData, Meta]
type NetLayers'       = '[Type, Succs]
type NetNode          = NetLayers  :<: Draft Static
type NetNode'         = NetLayers' :<: Draft Static
type NetRawNode       = NetLayers  :<: Raw
type NetCluster       = NetClusterLayers :< SubGraph NetNode
type NetCluster'      = NetClusterLayers :< SubGraph NetNode'
type NetRawCluster    = NetClusterLayers :< SubGraph NetRawNode

type NetGraph   = Hetero (VectorGraph NetRawNode (Link NetRawNode) NetRawCluster)
type NetGraph'  = Hetero (VectorGraph NetNode    (Link NetNode)    NetCluster)

type NetGraph'' = Hetero (VectorGraph NetNode'   (Link NetNode')   NetCluster')

buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM' (def :: NetGraph)

rebuildNetwork' = runIdentity .: rebuildNetworkM'
rebuildNetworkM' (net :: NetGraph) = flip Self.evalT (undefined ::        Ref Node NetNode)
                                     ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode))
                                     ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref Edge c)
                                     ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode)
                                     ∘ flip GraphBuilder.runT net
                                     ∘ registerSuccs   CONNECTION
{-# INLINE   buildNetworkM #-}
{-# INLINE rebuildNetworkM' #-}


class NetworkBuilderT net m n | m -> n, m -> net where runNetworkBuilderT :: net -> m a -> n (a, net)

instance {-# OVERLAPPABLE #-} NetworkBuilderT I IM IM where runNetworkBuilderT = impossible
instance {-# OVERLAPPABLE #-}
    ( m9 ~ Listener NODE_REMOVE       MemberRemove   m8
    , m8 ~ Listener CONNECTION_REMOVE SuccUnregister m7
    , m7 ~ Listener SUBGRAPH_INCLUDE  MemberRegister m6
    , m6 ~ Listener CONNECTION        SuccRegister   m5
    , m5 ~ GraphBuilder.BuilderT (Hetero (VectorGraph n e c)) m4
    , m4 ~ Listener ELEMENT (TypeConstraint Equality_Full (Ref Node NetNode)) m3
    , m3 ~ Listener CONNECTION (TypeConstraint Equality_M1 (Ref Edge c)) m2
    , m2 ~ Type.TypeBuilderT (Ref Node NetNode) m1
    , m1 ~ Self.SelfBuilderT (Ref Node NetNode) m
    , Monad m
    , net ~ Hetero (VectorGraph n e c)
    ) => NetworkBuilderT net m9 m where
    runNetworkBuilderT net = flip Self.evalT (undefined ::        Ref Node NetNode)
                           ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode))
                           ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref Edge c)
                           ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode)
                           ∘ flip GraphBuilder.runT net
                           ∘ registerSuccs   CONNECTION
                           ∘ registerMembers SUBGRAPH_INCLUDE
                           ∘ unregisterSuccs CONNECTION_REMOVE
                           ∘ removeMembers   NODE_REMOVE

runNetworkBuilderT' net = flip Self.evalT (undefined ::        Ref Node NetNode')
                        ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode'))
                        ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref Edge c)
                        ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode')
                        ∘ flip GraphBuilder.runT net
                        ∘ registerSuccs   CONNECTION
                        ∘ registerMembers SUBGRAPH_INCLUDE
                        ∘ unregisterSuccs CONNECTION_REMOVE
                        ∘ removeMembers   NODE_REMOVE


-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
-- FIXME[WD]: inputs should be more general and should be refactored out
inputstmp :: forall layout term rt x.
      (MapTryingElemList_
                            (Elems term (ByRuntime rt Lit.String x) x)
                            (TFoldable x)
                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]
inputstmp a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a



type instance Prop Inputs (Term layout term rt) = [Layout layout term rt]
instance (MapTryingElemList_
                           (Elems
                              term
                              (ByRuntime rt Lit.String (Layout layout term rt))
                              (Layout layout term rt))
                           (TFoldable (Layout layout term rt))
                           (Term layout term rt)) => Getter Inputs (Term layout term rt) where getter _ = inputstmp

type HasInputs n e = (OverElement (MonoTFunctor e) (RecordOf n), HasRecord n)

fmapInputs :: HasInputs n e => (e -> e) -> (n -> n)
fmapInputs (f :: e -> e) a = a & record %~ overElement (p :: P (MonoTFunctor e)) (monoTMap f)

