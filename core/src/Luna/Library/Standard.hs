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

import Luna.Syntax.AST.Decl.Function                (Function(..), FunctionPtr(..))
import Luna.Syntax.AST.Term
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetworkBuilderT, NetLayers, NetGraph)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term

import qualified Data.Map as Map

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Decl.Function    as Function

#define FunBuilderCtx(m) ( n ~ (NetLayers a :<: Draft Static)             \
                         , nodeRef ~ Ref Node n                          \
                         , TermNode Cons   m n                           \
                         , TermNode Blank  m n                           \
                         , TermNode Native m n                           \
                         , TermNode Star   m n                           \
                         , TermNode Var    m n                           \
                         , MonadFix m                                    \
                         , NetworkBuilderT (NetGraph a) m Identity       \
                         , MonadBuilder (NetGraph a) m                   \
                         )

buildGraph :: forall m n a b nodeRef . FunBuilderCtx(m) => m b -> (b, NetGraph a)
buildGraph m = runIdentity $ runNetworkBuilderT def ((star :: m nodeRef) >> m)

makeFunction :: FunBuilderCtx(m) => m (Maybe nodeRef, [nodeRef], nodeRef) -> Function n (NetGraph a)
makeFunction bldr = Function (FunctionPtr self args out) body where
    ((self, args, out), body) = buildGraph bldr

typed :: FunBuilderCtx(m) => m nodeRef -> nodeRef -> m nodeRef
typed b t = do
    el <- b
    reconnect el (prop Type) t
    return el

makeNativeFun :: FunBuilderCtx(m) => String -> Maybe String -> [String] -> String -> m (Maybe nodeRef, [nodeRef], nodeRef)
makeNativeFun name selfTypeStr argTypesStr outTypeStr = do
    selfType <- mapM (cons . fromString) selfTypeStr
    argTypes <- mapM (cons . fromString) argTypesStr
    outType  <- cons $ fromString outTypeStr
    self <- mapM (typed blank) selfType
    args <- mapM (typed blank) argTypes
    let nativeArgs = maybeToList self ++ args
    native <- native (fromString name) nativeArgs `typed` outType
    return (self, args, native)


makeId :: FunBuilderCtx(m) => m (Maybe nodeRef, [nodeRef], nodeRef)
makeId = do
    tpV <- var "#idTp1"
    n   <- blank `typed` tpV
    return (Nothing, [n], n)

symbols :: Show a => SymbolMap (NetLayers a :<: Draft Static) (NetGraph a)
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b))
    [ ("Int.+",          makeNativeFun "(+)"                     (Just "Int")      ["Int"]           "Int"      )
    , ("Int.*",          makeNativeFun "(*)"                     (Just "Int")      ["Int"]           "Int"      )
    , ("Int.toString",   makeNativeFun "show"                    (Just "Int")      []                "String"   )
    , ("String.length",  makeNativeFun "length"                  (Just "String")   []                "Int"      )
    , ("replicate"   ,   makeNativeFun "replicate"               Nothing           ["Int", "Double"] "[Double]" )
    , ("Int.toDouble",   makeNativeFun "fromIntegral"            (Just "Int")      []                "Double"   )
    , ("[Int].toDouble", makeNativeFun "(map fromIntegral)"      (Just "[Int]")    []                "[Double]" )
    , ("zero2pi"     ,   makeNativeFun "[0.0,0.1 .. 3.14]"       Nothing           []                "[Double]" )
    , ("vsin"        ,   makeNativeFun "(map sin)"               Nothing           ["[Double]"]      "[Double]" )
    , ("sin"         ,   makeNativeFun "(sin)"                   Nothing           ["Double"]        "Double"   )
    , ("range"       ,   makeNativeFun "enumFromTo"              Nothing           ["Int", "Int"]    "[Int]"    )
    , ("[Double]./"  ,   makeNativeFun "(flip $ map . flip (/))" (Just "[Double]") ["Double"]        "[Double]" )
    , ("id"          ,   makeId)
    ]
