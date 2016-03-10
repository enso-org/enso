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

makeNativeFun :: FunBuilderCtx(m) => String -> Maybe String -> [String] -> String -> m (Signature nodeRef)
makeNativeFun name selfTypeStr argTypesStr outTypeStr = do
    selfType <- mapM ((flip cons []) . fromString) selfTypeStr
    argTypes <- mapM ((flip cons []) . fromString) argTypesStr
    outType  <- cons (fromString outTypeStr) []
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
    [ ("Int.+"         , makeNativeFun "(+)"                     (Just "Int")      ["Int"]           "Int"      )
    , ("Int.*"         , makeNativeFun "(*)"                     (Just "Int")      ["Int"]           "Int"      )
    , ("Int.toString"  , makeNativeFun "show"                    (Just "Int")      []                "String"   )
    , ("String.length" , makeNativeFun "length"                  (Just "String")   []                "Int"      )
    , ("String.+"      , makeNativeFun "(++)"                    (Just "String")   ["String"]        "String"   )
    , ("replicate"     , makeNativeFun "replicate"               Nothing           ["Int", "Double"] "[Double]" )
    , ("Int.toDouble"  , makeNativeFun "fromIntegral"            (Just "Int")      []                "Double"   )
    , ("[Int].toDouble", makeNativeFun "(map fromIntegral)"      (Just "[Int]")    []                "[Double]" )
    , ("zero2pi"       , makeNativeFun "[0.0,0.1 .. 3.14]"       Nothing           []                "[Double]" )
    , ("vsin"          , makeNativeFun "(map sin)"               Nothing           ["[Double]"]      "[Double]" )
    , ("sin"           , makeNativeFun "(sin)"                   Nothing           ["Double"]        "Double"   )
    , ("range"         , makeNativeFun "enumFromTo"              Nothing           ["Int", "Int"]    "[Int]"    )
    , ("[Double]./"    , makeNativeFun "(flip $ map . flip (/))" (Just "[Double]") ["Double"]        "[Double]" )
    , ("id"            , makeId)
    ]
