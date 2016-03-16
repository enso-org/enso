{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Library.Standard where

import Prelude.Luna

import Data.Construction
import Data.Maybe                                   (maybeToList)
import Data.Prop
import Data.Graph
import Data.Graph.Builder
import Control.Monad.State                          as State

import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Library.Symbol.Class                    (MonadSymbol, SymbolMap)

import Luna.Syntax.AST.Function                     (Function (..), Signature (..))
import Luna.Syntax.AST.Term
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetworkBuilderT, NetLayers, NetGraph)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term

import qualified Luna.Syntax.AST.Term.Lit         as Lit
import qualified Data.Map                         as Map
import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Function         as Function

#define FunBuilderCtx(m) ( n ~ (NetLayers :<: Draft Static)      \
                         , nodeRef ~ Ref Node  n                 \
                         , TermNode Cons     m n                 \
                         , TermNode Blank    m n                 \
                         , TermNode Native   m n                 \
                         , TermNode Lit.Star m n                 \
                         , TermNode Lam      m n                 \
                         , TermNode Var      m n                 \
                         , TermNode App      m n                 \
                         , MonadFix m                            \
                         , NetworkBuilderT NetGraph m0 Identity  \
                         , MonadBuilder NetGraph m               \
                         , Monad m0                              \
                         , Destructor m (Ref Edge $ Link n)      \
                         , m ~ StateT (Map.Map TPRep nodeRef) m0 \
                         )

data TPRep = TCons String [TPRep] | TVar String deriving (Show, Eq, Ord)

listOf :: TPRep -> TPRep
listOf t = TCons "List" [t]

scons :: String -> TPRep
scons = flip TCons []

buildGraph :: forall m0 m n b nodeRef . FunBuilderCtx(m) => m b -> (b, NetGraph)
buildGraph m = runIdentity $ runNetworkBuilderT def $ evalStateT ((star :: m nodeRef) >> m) def

{-makeFunction :: FunBuilderCtx(m) => m (Signature nodeRef) -> Function (Ref Node n) NetGraph-}
makeFunction bldr = Function signature body where
    (signature, body) = buildGraph bldr

typed :: FunBuilderCtx(m) => m nodeRef -> nodeRef -> m nodeRef
typed b t = do
    el <- b
    reconnect (prop Type) el t
    return el

getTypeRep :: FunBuilderCtx(m) => TPRep -> m nodeRef
getTypeRep tp = do
    m <- State.get
    case Map.lookup tp m of
        Just r  -> return r
        Nothing -> do
            r <- case tp of
                TVar name -> var . fromString $ name
                TCons n as -> mapM (fmap arg . getTypeRep) as >>= cons (fromString n)
            State.put $ Map.insert tp r m
            return r

makeNativeFun3 :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> m (Signature nodeRef)
makeNativeFun3 funName = makeNativeFun ("((.).(.).(.)) return " <> funName)

makeNativeFun2 :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> m (Signature nodeRef)
makeNativeFun2 funName = makeNativeFun ("((.).(.)) return " <> funName)

makeNativeFun1 :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> m (Signature nodeRef)
makeNativeFun1 funName = makeNativeFun ("(.) return " <> funName)

makeNativeFun :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> m (Signature nodeRef)
makeNativeFun name selfTypeRep argTypesRep outTypeRep = do
    selfType <- mapM getTypeRep selfTypeRep
    argTypes <- mapM getTypeRep argTypesRep
    outType  <- getTypeRep outTypeRep
    self     <- mapM (typed blank) selfType
    args     <- mapM (typed blank) argTypes
    let nativeArgTypes = maybeToList selfType <> argTypes
    let nativeArgs = maybeToList self <> args
    lambda   <- lam (arg <$> nativeArgTypes) outType
    native   <- native (fromString name) `typed` lambda
    out      <- app native (arg <$> nativeArgs) `typed` outType
    return $ Signature self (arg <$> args) out

makeId :: FunBuilderCtx(m) => m (Signature nodeRef)
makeId = do
    tpV   <- var "#idTp1"
    n     <- blank `typed` tpV
    return $ Signature Nothing [arg n] n

symbols :: SymbolMap (NetLayers :<: Draft Static) NetGraph
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b))
    [ ("Int.+"         , makeNativeFun2 "(+)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.*"         , makeNativeFun2 "(*)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.-"         , makeNativeFun2 "(-)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int./"         , makeNativeFun2 "div"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.%"         , makeNativeFun2 "mod"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.^"         , makeNativeFun2 "(^)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.negate"    , makeNativeFun1 "negate"                  (Just $ scons "Int")    []              (scons "Int" ))
    , ("Int.abs"       , makeNativeFun1 "abs"                     (Just $ scons "Int")    []              (scons "Int" ))
    , ("Int.signum"    , makeNativeFun1 "signum"                  (Just $ scons "Int")    []              (scons "Int" ))
    , ("Int.pred"      , makeNativeFun1 "pred"                    (Just $ scons "Int")    []              (scons "Int" ))
    , ("Int.succ"      , makeNativeFun1 "succ"                    (Just $ scons "Int")    []              (scons "Int" ))

    , ("Int.=="        , makeNativeFun2 "(==)"                    (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int./="        , makeNativeFun2 "(/=)"                    (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int.<"         , makeNativeFun2 "(<)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int.<="        , makeNativeFun2 "(<=)"                    (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int.>"         , makeNativeFun2 "(>)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int.>="        , makeNativeFun2 "(>=)"                    (Just $ scons "Int")    [scons "Int"]   (scons "Bool"))
    , ("Int.odd"       , makeNativeFun1 "odd"                     (Just $ scons "Int")    []              (scons "Bool"))
    , ("Int.even"      , makeNativeFun1 "even"                    (Just $ scons "Int")    []              (scons "Bool"))

    , ("Int.min"       , makeNativeFun2 "min"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.max"       , makeNativeFun2 "max"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.gcd"       , makeNativeFun2 "gcd"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))
    , ("Int.lcm"       , makeNativeFun2 "lcm"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int" ))

    , ("Int.toString"  , makeNativeFun1 "show"                    (Just $ scons "Int")    []              (scons "String"))
    , ("Int.toDouble"  , makeNativeFun1 "fromIntegral"            (Just $ scons "Int")    []              (scons "Double"))

    , ("Int.times"     , makeNativeFun2 "replicate"               (Just $ scons "Int")    [TVar "#times"] (listOf $ TVar "#times"))
    , ("Int.upto"      , makeNativeFun2 "enumFromTo"              (Just $ scons "Int")    [scons "Int"]   (listOf $ scons "Int"))

    , ("String.length" , makeNativeFun1 "length"                  (Just $ scons "String") []               (scons "Int"))
    , ("String.+"      , makeNativeFun2 "(++)"                    (Just $ scons "String") [scons "String"] (scons "String"))

    , ("empty"         , makeNativeFun1 "([])"                    Nothing                        []                     (listOf $ TVar "#empty"))
    , ("List.length"   , makeNativeFun1 "length"                  (Just $ listOf $ TVar "#len")  []                     (scons "Int"))
    , ("List.+"        , makeNativeFun2 "(++)"                    (Just $ listOf $ TVar "#lpl")  [listOf $ TVar "#lpl"] (listOf $ TVar "#lpl"))

    , ("readFile"      , makeNativeFun  "readFile"                Nothing                        [scons "String"]       (scons "String"))
    , ("id"            , makeId)
    , ("if_then_else"  , makeNativeFun3 "(\\x y z -> if x then y else z)" Nothing [scons "Bool", TVar "#if", TVar "#if"] (TVar "#if"))
    ]
