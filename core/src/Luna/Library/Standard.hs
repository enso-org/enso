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
    [ ("Int.+"         , makeNativeFun "(+)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int"   ))
    , ("Int.*"         , makeNativeFun "(*)"                     (Just $ scons "Int")    [scons "Int"]   (scons "Int"   ))
    , ("Int.toString"  , makeNativeFun "show"                    (Just $ scons "Int")    []              (scons "String"))
    , ("Int.toDouble"  , makeNativeFun "fromIntegral"            (Just $ scons "Int")    []              (scons "Double"))
    , ("Int.times"     , makeNativeFun "replicate"               (Just $ scons "Int")    [TVar "#times"] (listOf $ TVar "#times"))
    , ("Int.upto"      , makeNativeFun "enumFromTo"              (Just $ scons "Int")    [scons "Int"]   (listOf $ scons "Int"))

    , ("String.length" , makeNativeFun "length"                  (Just $ TCons "String" []) []                  (TCons "Int"    []))
    , ("String.+"      , makeNativeFun "(++)"                    (Just $ TCons "String" []) [TCons "String" []] (TCons "String" []))

    , ("empty"         , makeNativeFun "([])"                    Nothing                        []                     (listOf $ TVar "#empty"))
    , ("List.length"   , makeNativeFun "length"                  (Just $ listOf $ TVar "#len")  []                     (scons "Int"))
    , ("List.+"        , makeNativeFun "(++)"                    (Just $ listOf $ TVar "#lpl")  [listOf $ TVar "#lpl"] (listOf $ TVar "#lpl"))

    , ("id"            , makeId)
    ]
