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

#define FunBuilderCtx(m) ( n ~ (NetLayers :<: Draft Static)    \
                         , nodeRef ~ Ref Node  n               \
                         , TermNode Cons     m n               \
                         , TermNode Blank    m n               \
                         , TermNode Native   m n               \
                         , TermNode Lit.Star m n               \
                         , TermNode Lam      m n               \
                         , TermNode Var      m n               \
                         , TermNode App      m n               \
                         , MonadFix m                          \
                         , NetworkBuilderT NetGraph m Identity \
                         , MonadBuilder NetGraph m             \
                         , Destructor m (Ref Edge $ Link n)    \
                         )

data TPRep = TCons String [TPRep] | TVar String

listOf :: TPRep -> TPRep
listOf t = TCons "List" [t]

buildGraph :: forall m n b nodeRef . FunBuilderCtx(m) => m b -> (b, NetGraph)
buildGraph m = runIdentity $ runNetworkBuilderT def ((star :: m nodeRef) >> m)

makeFunction :: FunBuilderCtx(m) => m (Signature nodeRef) -> Function (Ref Node n) NetGraph
makeFunction bldr = Function signature body where
    (signature, body) = buildGraph bldr

typed :: FunBuilderCtx(m) => m nodeRef -> nodeRef -> m nodeRef
typed b t = do
    el <- b
    reconnect (prop Type) el t
    return el

buildType :: FunBuilderCtx(m) => TPRep -> m nodeRef
buildType (TCons name args) = mapM (fmap arg . buildType) args >>= cons (fromString name)
buildType (TVar name)       = var . fromString $ name

makeNativeFun :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> m (Signature nodeRef)
makeNativeFun name selfTypeRep argTypesRep outTypeRep = do
    selfType <- mapM buildType selfTypeRep
    argTypes <- mapM buildType argTypesRep
    outType  <- buildType outTypeRep
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

makeReplicate :: FunBuilderCtx(m) => m (Signature nodeRef)
makeReplicate = do
    tpV <- var "#replicateA"
    tpL <- cons "List" [arg tpV]
    tpI <- cons "Int"  []
    in1 <- blank `typed` tpI
    in2 <- blank `typed` tpV
    l   <- lam [arg tpI, arg tpV] tpL
    n   <- native "replicate" `typed` l
    out <- app n [arg in1, arg in2] `typed` tpL
    return $ Signature Nothing [arg in1, arg in2] out

symbols :: SymbolMap (NetLayers :<: Draft Static) NetGraph
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b))
    [ ("Int.+"         , makeNativeFun "(+)"                     (Just $ TCons "Int" [])    [TCons "Int" []] (TCons "Int"    []))
    , ("Int.*"         , makeNativeFun "(*)"                     (Just $ TCons "Int" [])    [TCons "Int" []] (TCons "Int"    []))
    , ("Int.toString"  , makeNativeFun "show"                    (Just $ TCons "Int" [])    []               (TCons "String" []))
    , ("Int.toDouble"  , makeNativeFun "fromIntegral"            (Just $ TCons "Int" [])    []               (TCons "Double" []))

    , ("String.length" , makeNativeFun "length"                  (Just $ TCons "String" []) []                  (TCons "Int"    []))
    , ("String.+"      , makeNativeFun "(++)"                    (Just $ TCons "String" []) [TCons "String" []] (TCons "String" []))

    , ("List.length"   , makeNativeFun "length"                  (Just $ listOf $ TVar "#lenA") [] (TCons "Int" []))

    , ("replicate"     , makeReplicate)
    , ("range"         , makeNativeFun "enumFromTo"              Nothing [TCons "Int" [], TCons "Int" []]     (listOf $ TCons "Int" []))
    , ("id"            , makeId)
    ]
