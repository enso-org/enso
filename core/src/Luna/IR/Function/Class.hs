module Luna.IR.Function.Class where

import           Luna.Prelude
import           Luna.IR
import qualified Data.Map as Map
import           Data.Maybe (isJust, fromJust)
import           Data.TypeVal
import qualified Luna.IR.Internal.LayerStore as Store

data CompiledFunction = CompiledFunction { _ir   :: IR
                                         , _root :: AnyExpr
                                         }

makeLenses ''CompiledFunction

compile :: forall l m. (IRMonad m, Accessibles m '[ExprLinkNet, ExprNet, ExprLayer Succs, ExprLayer Type, ExprLayer Model, ExprLinkLayer Model])
               => AnyExpr -> m CompiledFunction
compile expr = do
    ir <- getIR >>= freeze
    return $ CompiledFunction ir expr

importFunction :: forall l m. (IRMonad m, Accessibles m '[ExprLinkNet, ExprNet, ExprLayer Succs, ExprLayer Type, ExprLayer Model, ExprLinkLayer Model])
               => CompiledFunction -> m ()
importFunction (CompiledFunction (IR map) _) = do
    ir      <- getIR
    exprNet <- readNet @EXPR
    linkNet <- readNet @(LINK' EXPR)
    let foreignExprs = fromJust $ Map.lookup (typeVal'_ @EXPR)         map -- until I can throw errors here
        foreignLinks = fromJust $ Map.lookup (typeVal'_ @(LINK' EXPR)) map
    exprTranslation <- Store.unsafeMerge foreignExprs exprNet
    linkTranslation <- Store.unsafeMerge foreignLinks linkNet
    let exprsToFix :: [AnyExpr]     = Elem . snd <$> exprTranslation
    let linksToFix :: [AnyExprLink] = Elem . snd <$> linkTranslation
    return ()


