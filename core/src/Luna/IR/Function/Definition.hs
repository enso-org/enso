{-# NoMonomorphismRestriction #-}

module Luna.IR.Function.Definition where

import           Luna.Prelude
import           Luna.IR
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe (isJust, fromJust)
import           Data.TypeDesc
import qualified Luna.IR.Internal.LayerStore as Store
import qualified Data.Vector.Unboxed         as Vector
import           Data.Vector.Unboxed         (Vector)

data CompiledFunction = CompiledFunction { _ir   :: IR
                                         , _root :: SomeExpr
                                         }

makeLenses ''CompiledFunction

compile :: forall l m. (MonadIR m, Editors Net '[AnyExpr, AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExpr // Type, AnyExpr // Model, AnyExprLink // Model] m)
               => SomeExpr -> m CompiledFunction
compile expr = do
    ir <- getIR >>= freeze
    return $ CompiledFunction ir expr

mkTranslationVector :: Int -> [(Int, Int)] -> Vector Int
mkTranslationVector size knownIxes = Vector.fromList reixed where
    reixed = go size knownIxes
    go 0 _                = []
    go i []               = 0 : go (i - 1) []
    go i l@((j, v) : es)
        | (size - i) == j = v : go (i - 1) es
        | otherwise       = 0 : go (i - 1) l

translateWith :: Vector Int -> (forall t. Elem t -> Elem t)
translateWith v = idx %~ Vector.unsafeIndex v

importFunction :: forall l m. (MonadIR m, MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Emitter (Import // AnyExpr) m, Emitter (Import // AnyExprLink) m)
               => CompiledFunction -> m SomeExpr
importFunction (CompiledFunction (unwrap' -> map) r) = do
    ir      <- getIR
    exprNet <- readNet @AnyExpr
    linkNet <- readNet @AnyExprLink
    let foreignExprs = fromJust $ Map.lookup (getTypeDesc @AnyExpr)     map -- until I can throw errors here
        foreignLinks = fromJust $ Map.lookup (getTypeDesc @AnyExprLink) map
    exprTrans <- Store.unsafeMerge foreignExprs exprNet
    linkTrans <- Store.unsafeMerge foreignLinks linkNet

    let exprTranslator :: forall t.   Expr t -> Expr t
        exprTranslator =  translateWith $ mkTranslationVector (foreignExprs ^. Store.size) exprTrans
        linkTranslator :: forall a b. ExprLink a b -> ExprLink a b
        linkTranslator =  translateWith $ mkTranslationVector (foreignLinks ^. Store.size) linkTrans
    let importedExprs :: [SomeExpr]     = Elem . snd <$> exprTrans
        importedLinks :: [SomeExprLink] = Elem . snd <$> linkTrans

    forM_ importedLinks $ emit . Payload @(Import // AnyExprLink) . (, ElemTranslations exprTranslator linkTranslator) . unsafeGeneralize
    forM_ importedExprs $ emit . Payload @(Import // AnyExpr)     . (, ElemTranslations exprTranslator linkTranslator) . unsafeGeneralize
    return $ exprTranslator r
