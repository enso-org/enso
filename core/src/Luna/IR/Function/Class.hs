module Luna.IR.Function.Class where

import           Luna.Prelude
import           Luna.IR
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe (isJust, fromJust)
import           Data.TypeVal
import qualified Luna.IR.Internal.LayerStore as Store
import qualified Data.Vector.Unboxed         as Vector
import           Data.Vector.Unboxed         (Vector)

data CompiledFunction = CompiledFunction { _ir   :: IR
                                         , _root :: AnyExpr
                                         }

makeLenses ''CompiledFunction

compile :: forall l m. (IRMonad m, Accessibles m '[ExprLinkNet, ExprNet, ExprLayer Succs, ExprLayer Type, ExprLayer Model, ExprLinkLayer Model])
               => AnyExpr -> m CompiledFunction
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

translateWith :: IsIdx t => Vector Int -> t -> t
translateWith v = idx %~ Vector.unsafeIndex v

importFunction :: forall l m. (IRMonad m, Accessibles m '[ExprLinkNet, ExprNet, ExprLayer Succs, ExprLayer Type, ExprLayer Model, ExprLinkLayer Model])
               => CompiledFunction -> m ()
importFunction (CompiledFunction (IR map) _) = do
    ir      <- getIR
    exprNet <- readNet @EXPR
    linkNet <- readNet @(LINK' EXPR)
    let foreignExprs = fromJust $ Map.lookup (typeVal'_ @EXPR)         map -- until I can throw errors here
        foreignLinks = fromJust $ Map.lookup (typeVal'_ @(LINK' EXPR)) map
    exprTrans <- Store.unsafeMerge foreignExprs exprNet
    linkTrans <- Store.unsafeMerge foreignLinks linkNet

    let exprTranslator              = translateWith $ mkTranslationVector (foreignExprs ^. Store.size) exprTrans
        linkTranslator              = translateWith $ mkTranslationVector (foreignLinks ^. Store.size) linkTrans
        exprsToFix :: [AnyExpr]     = Elem . snd <$> exprTrans
        linksToFix :: [AnyExprLink] = Elem . snd <$> linkTrans

    forM_ linksToFix $ modifyLayer_ @Model $ over both exprTranslator
    forM_ exprsToFix $ \e -> do
         inplaceModifyFieldsWith linkTranslator           e
         modifyLayer_ @Type      linkTranslator           e
         modifyLayer_ @Succs     (Set.map linkTranslator) e
    return ()


