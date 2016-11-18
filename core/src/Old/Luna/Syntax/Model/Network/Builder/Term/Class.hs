{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

{-# LANGUAGE PolyKinds            #-}

module Old.Luna.Syntax.Model.Network.Builder.Term.Class (module Old.Luna.Syntax.Model.Network.Builder.Term.Class, module X) where

import Prelude.Luna    hiding (Num, curry, Curry, Type)
import qualified Prelude.Luna as P
import Prologue.Unsafe (error)
import Data.Typeable.Proxy.Abbr (P, p)

import           Control.Monad.Event
import           Data.Graph
import           Data.Graph.Builders
import           Data.Layer_OLD.Cover_OLD
import           Old.Data.Prop
import qualified Data.Record                             as Record
import           Data.Record                             (RecordOf, HasRecord, record, MapTryingElemList_, withElement_, OverElement, overElement)
import           Luna.Runtime.Dynamics                 as Runtime
import           Luna.IR.Function.Argument
import           Luna.IR.Function.Argument       as X (arg)
import           Old.Luna.Syntax.Term.Class                    hiding (undefined, Source, Name)
import qualified Data.Graph.Builder                      as GraphBuilder
import           Old.Luna.Syntax.Model.Layer                 (Type, TCData, Meta, Name, Lambda, (:<:), (:<))
import           Luna.Interpreter.Layer (InterpreterData)
import           Old.Luna.Syntax.Model.Network.Builder.Layer
import qualified Old.Luna.Syntax.Model.Network.Builder.Self  as Self
import qualified Old.Luna.Syntax.Model.Network.Builder.Type  as Type
import           Old.Luna.Syntax.Model.Network.Term
import qualified Old.Luna.Syntax.Term.Expr.Lit                as Lit
import           Control.Monad.Trans.Identity
import           Old.Luna.Syntax.Term.Expr.Lit               (Star(Star))
import qualified Data.Graph.Backend.NEC as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import Data.Graph.Builder (write)
import qualified Control.Monad.State as State
import Control.Monad.Primitive (PrimState, PrimMonad, primitive)

import qualified Old.Luna.Syntax.Term as New
-- import           Old.Luna.Syntax.Term (AtomicExpr, TermOf)
import           Old.Luna.Syntax.Term.Class (TermOf)
import           Data.Container.Hetero (Elems)
import qualified Luna.IR.Expr.Atom as Atom
import GHC.Prim (Any)


-------------------------------------
-- === Term building utilities === --
-------------------------------------

-- === Utility Type Families === --

-- FIXME[WD]: Zmienic nazwe Layout na adekwatna
-- FIXME[WD]: Skoro Layout okresla jakie jest "wejscie" do nodu, to Input nie jest potrzebny, bo mozna go wyinferowac jako odwrotnosc Connection

type family BuildArgs (t :: k) n :: *
type family BuildArgs2 t n :: *
type family Expanded  (t :: k) n :: *


-- === ElemBuilder_OLD === --

class    ElemBuilder_OLD el m  a where buildElem :: el -> m a
instance {-# OVERLAPPABLE #-} ElemBuilder_OLD el IM a where buildElem = impossible


class    ElemBuilder2 el m  a where buildElem2 :: el -> m a
instance {-# OVERLAPPABLE #-} ElemBuilder2 el IM a where buildElem2 = impossible


instance {-# OVERLAPPABLE #-}
         ( Record.Cons el (Uncovered a)
         , CoverConstructor m a
         , Dispatcher ELEMENT a m
         , Self.MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder_OLD el m a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem el = dispatch ELEMENT =<< Self.buildAbsMe (constructCover $ Record.cons el)
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
    buildElem2 el = dispatch ELEMENT =<< Self.buildAbsMe (constructCover el)
    {-# INLINE buildElem2 #-}


-- === TermBuilder_OLD === --

class TermBuilder_OLD (t :: k) m a where buildTerm_OLD :: Proxy t -> BuildArgs t a -> m a
instance {-# OVERLAPPABLE #-} TermBuilder_OLD t IM a where buildTerm_OLD = impossible



class Dispatcher ELEMENT a m => TermBuilder t m a where buildTerm :: t -> BuildArgs2 t a -> m a
instance {-# OVERLAPPABLE #-} Dispatcher ELEMENT a IM => TermBuilder t IM a where buildTerm = impossible


-- class TermBuilder2 t fmt dyn a where buildTerm2 :: a -> BuildArgs2 a (AtomicExpr t fmt dyn a) -> m (AtomicExpr t fmt dyn a)
-- newtype     Term2   t fmt dyn a = Term2 (Layout2 t fmt dyn a)



type family TermBuilders ts m a :: Constraint where
            TermBuilders '[] m a = ()
            TermBuilders (t ': ts) m a = (TermBuilder t m a, TermBuilders ts m a)


type ExprBuilder fmt m a = TermBuilders (Elems fmt) m a

build :: TermBuilder t m a => t -> BuildArgs2 t a -> m a
build t args = buildTerm t args >>= dispatch ELEMENT ; {-# INLINE build #-}




newtype BindBuilder t m a = BindBuilder (IdentityT m a) deriving (Functor, Applicative, Monad, MonadTrans)
makeWrapped ''BindBuilder

{-ghc8 dirty fix-}
instance Show (BindBuilder t m a) where
    show _ = "ghc8 dirty fix in Luna.Syntax.Model.Network.Builder.Term.Class"

runBindBuilder :: BindBuilder t m t -> m t
runBindBuilder = runIdentityT ∘ unwrap'

instance PrimMonad m => PrimMonad (BindBuilder t m) where
    type PrimState (BindBuilder t m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


-------------------------------
-- === Term constructors === --
-------------------------------


-- === Lit === --

type instance BuildArgs Lit.Star   n = ()
type instance BuildArgs Lit.String n = OneTuple Lit.String
type instance BuildArgs Lit.Number n = OneTuple Lit.Number

instance ElemBuilder_OLD Lit.Star   m a => TermBuilder_OLD Lit.Star   m a where buildTerm_OLD p ()           = buildElem Lit.Star
instance ElemBuilder_OLD Lit.String m a => TermBuilder_OLD Lit.String m a where buildTerm_OLD p (OneTuple s) = buildElem s
instance ElemBuilder_OLD Lit.Number m a => TermBuilder_OLD Lit.Number m a where buildTerm_OLD p (OneTuple s) = buildElem s

star :: TermBuilder_OLD Lit.Star m a => m a
star = P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.Star)

str :: TermBuilder_OLD Lit.String m a => String -> m a
str = (P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.String)) ∘ Lit.String

ratio :: TermBuilder_OLD Lit.Number m a => Rational -> m a
ratio = (P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Rational

int :: TermBuilder_OLD Lit.Number m a => Integer -> m a
int = (P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Integer

double :: TermBuilder_OLD Lit.Number m a => Double -> m a
double = (P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.Number)) ∘ Lit.decimal ∘ Lit.Double

number :: TermBuilder_OLD Lit.Number m a => Lit.Number -> m a
number = (P.curry $ buildTerm_OLD (Proxy :: Proxy Lit.Number))

-- === Val === --

type instance BuildArgs Cons n = (NameInput n, [Arg (Input n)])
type instance BuildArgs Lam  n = ([Arg (Input n)], Input n)



instance ( name ~ NameInput a
         , inp ~ Input a
         , MonadFix m
         , Connectible     inp  a m
         , ConnectibleName name a m
         , ElemBuilder_OLD (Cons (NameConnection name a) (Ref Edge (Connection inp a))) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , LayerConstructor m a
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Cons m a where
    buildTerm_OLD p (name, args) = mdo
        out   <- buildElem $ Cons cname cargs
        cname <- nameConnection name out
        cargs <- (mapM ∘ mapM) (flip connection out) args
        return out

instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder_OLD (Lam $ Ref Edge (Connection inp a)) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , LayerConstructor m a
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Lam m a where
    buildTerm_OLD p (args, res) = mdo
        out   <- buildElem $ Lam cargs cres
        cargs <- (mapM ∘ mapM) (flip connection out) args
        cres  <- connection res out
        return out


cons :: TermBuilder_OLD Cons m a => NameInput a -> [Arg $ Input a] -> m a
cons = P.curry $ buildTerm_OLD (Proxy :: Proxy Cons)

lam :: TermBuilder_OLD Lam m a => [Arg $ Input a] -> Input a -> m a
lam = P.curry $ buildTerm_OLD (Proxy :: Proxy Lam)


-- === Thunk === --

type instance BuildArgs Acc    n = (NameInput n, Input n)
type instance BuildArgs App    n = (Input n, [Arg (Input n)])
type instance BuildArgs Curry  n = (Input n, [Arg (Input n)])
type instance BuildArgs Native n = OneTuple (NameInput n)

instance {-# OVERLAPPABLE #-}
         ( inp  ~ Input a
         , name ~ NameInput a
         , MonadFix m
         , Connectible     inp  a m
         , ConnectibleName name a m
         , ElemBuilder_OLD (Acc (NameConnection name a) (Ref Edge (Connection inp a))) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Acc m a where
    buildTerm_OLD p (name, src) = mdo
        out   <- buildElem $ Acc cname csrc
        cname <- nameConnection name out
        csrc  <- connection     src  out
        return out

instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder_OLD (App $ Ref Edge (Connection inp a)) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD App m a where
    buildTerm_OLD p (src, args) = mdo
        out   <- buildElem $ App csrc cargs
        csrc  <- connection src out
        cargs <- (mapM ∘ mapM) (flip connection out) args
        return out

instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder_OLD (Curry $ Ref Edge (Connection inp a)) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Curry m a where
    buildTerm_OLD p (src, args) = mdo
        out   <- buildElem $ Curry csrc cargs
        csrc  <- connection src out
        cargs <- (mapM ∘ mapM) (flip connection out) args
        return out

instance ( name ~ NameInput a
         , MonadFix m
         , ConnectibleName name a m
         , ElemBuilder_OLD (Native $ NameConnection name a) m a
         ) => TermBuilder_OLD Native m a where
    buildTerm_OLD p (OneTuple name) = mdo
        out   <- buildElem $ Native cname
        cname <- nameConnection name out
        return out

acc :: TermBuilder_OLD Acc m a => NameInput a -> Input a -> m a
acc = P.curry $ buildTerm_OLD (Proxy :: Proxy Acc)

app :: TermBuilder_OLD App m a => Input a -> [Arg $ Input a] -> m a
app = P.curry $ buildTerm_OLD (Proxy :: Proxy App)

curry :: TermBuilder_OLD Curry m a => Input a -> [Arg $ Input a] -> m a
curry = P.curry $ buildTerm_OLD (Proxy :: Proxy Curry)

native :: TermBuilder_OLD Native m a => NameInput a -> m a
native = P.curry $ buildTerm_OLD (Proxy :: Proxy Native)

-- === Expr === --


type instance BuildArgs Var n = OneTuple (NameInput n)
instance ( name ~ NameInput a
         , MonadFix m
         , ConnectibleName name a m
         , ElemBuilder_OLD (Var $ NameConnection name a) m a
         ) => TermBuilder_OLD Var m a where
    buildTerm_OLD p (OneTuple name) = mdo
        out   <- buildElem $ Var cname
        cname <- nameConnection name out
        return out

type instance BuildArgs Unify n = (Input n, Input n)
instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder_OLD (Unify $ Ref Edge (Connection inp a)) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , LayerConstructor m a
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Unify m a where
    buildTerm_OLD p (a,b) = mdo
        out <- buildElem $ Unify ca cb
        ca  <- connection a out
        cb  <- connection b out
        return out

type instance BuildArgs Match n = (Input n, Input n)
instance ( inp ~ Input a
         , MonadFix m
         , Connectible inp a m
         , ElemBuilder_OLD (Match $ Ref Edge (Connection inp a)) m a
         {-ghc8-}
         , inp ~ Ref Node src
         , Uncovered a ~ Uncovered (Unlayered a)
         , LayerConstructor m a
         , Connectible' (Ref Node src) a m (Arc src src)
         ) => TermBuilder_OLD Match m a where
    buildTerm_OLD p (a,b) = mdo
        out <- buildElem $ Match ca cb
        ca  <- connection a out
        cb  <- connection b out
        return out


        --type instance BuildArgs Unify n = (Input n, Input n)
        --instance ( a ~ Input a
        --         , Record.Cons (Unify (Param a)) (TermOf a)
        --         , ElemBuilder3 n   a
        --         , Parametrized        n   a
        --         , ParamResolver          n m a
        --         ) => TermBuilder_OLD Unify m a where
        --    buildTerm_OLD p (a,b) = term $ unifyCons <$> param a <*> param b


        -- instance ElemBuilder_OLD Lit.Star   m a => TermBuilder_OLD Lit.Star   m a where buildTerm_OLD p ()           = buildElem Lit.Star
        -- instance ElemBuilder_OLD Lit.String m a => TermBuilder_OLD Lit.String m a where buildTerm_OLD p (OneTuple s) = buildElem s
        -- instance ElemBuilder_OLD Lit.Number m a => TermBuilder_OLD Lit.Number m a where buildTerm_OLD p (OneTuple s) = buildElem s
        --
        -- star :: TermBuilder_OLD Lit.Star m a => m a
        -- star = curry $ buildTerm_OLD (Proxy :: Proxy Lit.Star)


type instance BuildArgs2 Star a = ()
instance TermBuilderCtx Star n m a => TermBuilder Star m a where
    buildTerm p () = term $ pure starCons

type instance BuildArgs2 New.Var a = OneTuple (NameInput a)
instance TermBuilderCtx New.Var n m a => TermBuilder New.Var m a where
    buildTerm p (OneTuple a) = term $ varCons <$> nameParam a

type instance BuildArgs2 New.Match a = (a,a)
instance TermBuilderCtx New.Match n m a => TermBuilder New.Match m a where
    buildTerm p (a,b) = term $ matchCons <$> param a <*> param b

type instance BuildArgs2 New.Unify a = (a, a)
instance TermBuilderCtx New.Unify n m a => TermBuilder New.Unify m a where
    buildTerm p (a,b) = term $ unifyCons <$> param a <*> param b

starCons :: Record.Cons Star a => a
starCons  = Record.cons    Star

varCons :: Record.Cons (Var n) a => n -> a
varCons   = Record.cons ∘  Var
unifyCons = Record.cons ∘∘ Unify
matchCons = Record.cons ∘∘ Match

type family Parameterized t a
type instance Parameterized Star   a = Star
type instance Parameterized New.Var   a = Var   $ NameParam a
type instance Parameterized New.Unify a = Unify $ Param     a
type instance Parameterized New.Match a = Match $ Param     a


type TermBuilderCtx t n m a = ( Dispatcher ELEMENT a m
                              , Record.Cons (Parameterized t a) (TermOf a)
                              , ElemBuilder3  n   a
                              , Parametrized  n   a
                              , ParamResolver n m a
                              )

--type ElemBuilder3 a = SmartCons (Match (Param a)) (TermOf a)

--class TermBuilder_OLD (t :: k) m a where buildTerm_OLD :: Proxy t -> BuildArgs t a -> m a

--class

--term :: (ParamResolver n m a, ElemBuilder3 m a)
--             => BindBuilder a (n m) (TermOf a) -> m a

term :: (ElemBuilder3 n a, ParamResolver n m a) => BindBuilder a n (TermOf a) -> m a
term m = resolveParams $ runBindBuilder $ (lift ∘ buildElem3) =<< m

var :: TermBuilder_OLD Var m a => NameInput a -> m a
var = P.curry $ buildTerm_OLD (Proxy :: Proxy Var)

unify :: TermBuilder_OLD Unify m a => Input a -> Input a -> m a
unify = P.curry $ buildTerm_OLD (Proxy :: Proxy Unify)

match :: TermBuilder_OLD Match m a => Input a -> Input a -> m a
match = P.curry $ buildTerm_OLD (Proxy :: Proxy Match)

star2 :: TermBuilder Star m a => m a
star2 = P.curry $ build Star

-- var2 :: TermBuilder New.Var m a => NameInput a -> m a
-- var2 = P.curry $ build Atom.Var


-- star3 :: TermBuilder Star m a => m (Term2' t fmt dyn)
-- star3 = curry $ build Star



type family Param a
type family NameParam a
type family Source2 a



--class ElemBuilder3 m a where
--    nameParam   :: NameInput a -> m (NameParam a)
--    param       :: a -> m (Param a)
--    buildElem3 :: Source2 a -> m a

--class TransRunner n m

--class (MonadTrans n, Monad (n m), Monad m) => ParamResolver n m a | m a -> n where
--    resolveParams :: n m a -> m a

class (Monad n, Monad m) => ParamResolver n m a | m a -> n where
    resolveParams :: n a -> m a


--newtype Root a   = Root a   deriving (Show, Functor, Foldable, Traversable)
--newtype Slot a   = Slot a   deriving (Show, Functor, Foldable, Traversable)
--data    Bind a b = Bind a b deriving (Show)


--class ParamResolver t m a | t -> a where
--    resolveParam :: t -> m ()

--instance ( GraphBuilder.MonadBuilder (Hetero (VectorGraph node edge cluster)) m
--         , ParamResolver t m v
--         , State.MonadState [(a, Ref Edge a)] m
--         ) => ParamResolver (Bind t a) m v where
--    resolveParam (Bind t a) = do
--        cref <- reserveConnection
--        State.modify ((a, cref):)
--        resolveParam t -- dorobic aplikowanie trzeba







class Parametrized m t where
    nameParam   :: NameInput t -> BindBuilder t m (NameParam t)
    param       :: t           -> BindBuilder t m     (Param t)


class ElemBuilder3 m t where
    buildElem3 :: TermOf t -> m t


--instance ElemBuilder3 m t where buildElem3 = undefined

--class NamedConnectionReservation     src tgt m conn where reserveNamedConnection  ::             src -> Proxy tgt -> m conn

-- reserveConnectionM :: (MonadBuilder (Hetero (NEC.MGraph (PrimState m) n e c)) m, PrimMonad m) => m (Ref Edge a)

instance ( PrimMonad m, GraphBuilder.MonadBuilder (Hetero (NEC.MGraph (PrimState m) n e c)) m
         , NamedConnectionReservation (NameInput (Ref Node a)) (Ref Node a) m (NameParam (Ref Node a))
         , State.MonadState [(Ref Node a, Ref Edge (Link a))] m
         ) => Parametrized m (Ref Node a) where
    param t = do
        cref <- reserveConnectionM
        lift $ State.modify ((t, cref):) -- FIXME[WD]: remove lift
        return cref
    -- FIXME[WD]: add the logic for dynamic graphs to add pending connections to state, like above
    nameParam name = lift $ reserveNamedConnection name (p :: P (Ref Node a))
    {-# INLINE param     #-}
    {-# INLINE nameParam #-}


--write :: (MonadBuilder t m, Referred r a t) => Ref r a -> a -> m ()
--write ref = modify_ ∘ set (focus ref)



type instance TermOf (Ref t a) = TermOf a
type instance Param (Ref Node a) = Ref Edge (Link a)
type instance NameParam (Ref Node a) = Lit.String -- FIXME[WD]: Support dynamic terms

--Term t Val  Static


--Term t term rt

--class TermBuilder_OLD t term rt where
--    param ::


-- === Draft === --

type instance BuildArgs   Blank n = ()
instance      ElemBuilder_OLD Blank m a => TermBuilder_OLD Blank m a where buildTerm_OLD p () = buildElem Blank

blank :: TermBuilder_OLD Blank m a => m a
blank = P.curry $ buildTerm_OLD (Proxy :: Proxy Blank)






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
type NetCluster       = NetClusterLayers :< RefSet Node NetNode
type NetCluster'      = NetClusterLayers :< RefSet Node NetNode'
type NetRawCluster    = NetClusterLayers :< RefSet Node NetRawNode

type NetGraph   = Hetero (NEC.Graph NetRawNode (Link NetRawNode) NetRawCluster)
type NetGraph'  = Hetero (NEC.Graph NetNode    (Link NetNode)    NetCluster)

type NetGraph2  = Hetero (NEC.Graph Any Any Any)

type NetGraph'' = Hetero (NEC.Graph NetNode'   (Link NetNode')   NetCluster')

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
    , m5 ~ GraphBuilder.BuilderT (Hetero (NEC.Graph n e c)) m4
    , m4 ~ Listener ELEMENT (TypeConstraint Equality_Full (Ref Node NetNode)) m3
    , m3 ~ Listener CONNECTION (TypeConstraint Equality_Full (Ref Edge (Link NetNode))) m2
    , m2 ~ Type.TypeBuilderT (Ref Node NetNode) m1
    , m1 ~ Self.SelfBuilderT (Ref Node NetNode) m
    , Monad m
    , net ~ Hetero (NEC.Graph n e c)
    ) => NetworkBuilderT net m9 m where
    runNetworkBuilderT net = flip Self.evalT (undefined ::        Ref Node NetNode)
                           ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode))
                           ∘ constrainTypeEq CONNECTION (Proxy :: Proxy $ Ref Edge (Link NetNode))
                           ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode)
                           ∘ flip GraphBuilder.runT net
                           ∘ registerSuccs   CONNECTION
                           ∘ registerMembers SUBGRAPH_INCLUDE
                           ∘ unregisterSuccs CONNECTION_REMOVE
                           ∘ removeMembers   NODE_REMOVE

class NetworkBuilderT2 net m n | m -> n, m -> net where runNetworkBuilderT2 :: net -> m a -> n (a, net)

instance {-# OVERLAPPABLE #-} NetworkBuilderT2 I IM IM where runNetworkBuilderT2 = impossible
instance {-# OVERLAPPABLE #-}
    ( m9 ~ Listener NODE_REMOVE       MemberRemove   m8
    , m8 ~ Listener CONNECTION_REMOVE SuccUnregister m7
    , m7 ~ Listener SUBGRAPH_INCLUDE  MemberRegister m6
    , m6 ~ Listener CONNECTION        SuccRegister   m5
    , m5 ~ GraphBuilder.BuilderT (Hetero (NEC.MGraph (PrimState m) n e c)) m4
    , m4 ~ Listener ELEMENT (TypeConstraint Equality_Full (Ref Node NetNode)) m3
    , m3 ~ Listener CONNECTION (TypeConstraint Equality_Full (Ref Edge (Link NetNode))) m2
    , m2 ~ Type.TypeBuilderT (Ref Node NetNode) m1
    , m1 ~ Self.SelfBuilderT (Ref Node NetNode) m
    , PrimMonad m
    , net ~ Hetero (NEC.Graph n e c)
    ) => NetworkBuilderT2 net m9 m where
    runNetworkBuilderT2 net mf = do
        net' <- mapM NEC.unsafeThaw net
        (a, netout) <- flip Self.evalT (undefined ::        Ref Node NetNode)
                     ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode))
                     ∘ constrainTypeEq CONNECTION (Proxy :: Proxy $ Ref Edge (Link NetNode))
                     ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode)
                     ∘ flip GraphBuilder.runT net'
                     ∘ registerSuccs   CONNECTION
                     ∘ registerMembers SUBGRAPH_INCLUDE
                     ∘ unregisterSuccs CONNECTION_REMOVE
                     ∘ removeMembers   NODE_REMOVE
                     $ mf
        netout' <- mapM NEC.unsafeFreeze netout
        return (a, netout')

runNetworkBuilderT' net = flip Self.evalT (undefined ::        Ref Node NetNode')
                        ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode'))
                        ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref Edge c)
                        ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode')
                        ∘ flip GraphBuilder.runT net
                        ∘ registerSuccs   CONNECTION
                        ∘ registerMembers SUBGRAPH_INCLUDE
                        ∘ unregisterSuccs CONNECTION_REMOVE
                        ∘ removeMembers   NODE_REMOVE

runNetworkBuilderT_1 net = flip Self.evalT (undefined ::        Ref Node NetNode)
                         ∘ flip Type.evalT (Nothing   :: Maybe (Ref Node NetNode))
                         ∘ constrainTypeEq CONNECTION (Proxy :: Proxy $ Ref Edge (Link NetNode))
                         ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref Node NetNode)
                         ∘ flip GraphBuilder.runT net
                         ∘ registerSuccs   CONNECTION
                         ∘ registerMembers SUBGRAPH_INCLUDE
                         ∘ unregisterSuccs CONNECTION_REMOVE
                         ∘ removeMembers   NODE_REMOVE

-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
-- FIXME[WD]: inputs should be more general and should be refactored out
inputstmp :: forall layout term rt x.
      (MapTryingElemList_
                            (Elems_OLD term (ByDynamics rt Lit.String x) x)
                            (TFoldable x)
                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]
inputstmp a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a



type instance Prop Inputs (Term layout term rt) = [Layout layout term rt]
instance (MapTryingElemList_
                           (Elems_OLD
                              term
                              (ByDynamics rt Lit.String (Layout layout term rt))
                              (Layout layout term rt))
                           (TFoldable (Layout layout term rt))
                           (Term layout term rt)) => Getter Inputs (Term layout term rt) where getter _ = inputstmp

type HasInputs n e = (OverElement (MonoTFunctor e) (RecordOf n), HasRecord n)

fmapInputs :: HasInputs n e => (e -> e) -> (n -> n)
fmapInputs (f :: e -> e) a = a & record %~ overElement (p :: P (MonoTFunctor e)) (monoTMap f)
