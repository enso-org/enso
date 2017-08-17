{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Builtin.Data.Function where

import Luna.Prelude

import OCI.IR.Class         as IR
import Luna.IR              hiding (Function, Import)
import OCI.IR.Layer.Class
import Luna.IR.Layer.Succs
import Luna.IR.Layer.Type
import OCI.IR.Layer.Model
import OCI.IR.Name
import OCI.IR.Name.Qualified
import OCI.IR.Term
import Data.Event
import Data.Property

import           Control.Monad.State.Dependent
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe (isJust, fromJust)
import           Data.TypeDesc
import qualified Data.ManagedVectorMap       as Store
import qualified Data.Vector.Unboxed         as Vector
import           Data.Vector.Unboxed         (Vector)
import           Luna.Builtin.Data.LunaValue (LunaValue)

-- === Definition === --

data Assumptions = Assumptions { _unifies      :: [Expr Unify]
                               , _merges       :: [Expr Unify]
                               , _unsolvedApps :: [Expr Monadic]
                               , _unsolvedAccs :: [Expr Monadic]
                               } deriving (Show, Eq)
makeLenses ''Assumptions

instance Default Assumptions where
    def = Assumptions def def def def

data Function = Function { _header       :: Rooted SomeExpr
                         , _value        :: LunaValue
                         , _assumptions  :: Assumptions
                         }
makeLenses ''Function


-- === Compilation === --

compile :: forall l m. (MonadIR m, Editors Net '[AnyExpr, AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExpr // Type, AnyExpr // Model, AnyExprLink // Model] m)
               => SomeExpr -> m (Rooted SomeExpr)
compile expr = do
    ir <- fmap wrap . freeze . unwrap =<< get @IRST
    return $ Rooted ir expr

useCompiled :: MonadIR m => Rooted a -> m a
useCompiled (Rooted body e) = do
    ir <- wrap <$> thaw (unwrap body)
    put @IRST ir
    return e


-- === Importing === --

importFunction :: forall l m. (MonadIR m, MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Emitter (Import // AnyExpr) m, Emitter (Import // AnyExprLink) m)
               => Function -> m (SomeExpr, Assumptions)
importFunction (Function header value (Assumptions unis merges apps accs)) = do
    trans <- importRooted header
    let t :: forall r . Expr r -> Expr r
        t = unsafeGeneralize . trans . unsafeGeneralize
    return (trans $ header ^. IR.root, Assumptions (t <$> unis) (t <$> merges) (t <$> apps) (t <$> accs))

importRooted :: forall l r m. (MonadIR m, MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Emitter (Import // AnyExpr) m, Emitter (Import // AnyExprLink) m)
               => Rooted (Expr l) -> m (Expr r -> Expr r)
importRooted (Rooted ((_unwrap . _unwrap) -> map) _) = do -- FIXME[WD]: Why we need to use _unwrap here and unwrap does not work?
    exprNet <- readNet @AnyExpr
    linkNet <- readNet @AnyExprLink
    let foreignExprs = fromJust $ Map.lookup (getTypeDesc @AnyExpr)     map -- until I can throw errors here
        foreignLinks = fromJust $ Map.lookup (getTypeDesc @AnyExprLink) map
    (exprTrans, reinitExprs) <- Store.unsafeMerge foreignExprs exprNet
    (linkTrans, reinitLinks) <- Store.unsafeMerge foreignLinks linkNet

    let exprTranslator :: forall t.   Expr     t   -> Expr     t
        linkTranslator :: forall a b. ExprLink a b -> ExprLink a b
        exprTranslator =  translateWith $ mkTranslationVector (foreignExprs ^. Store.size) exprTrans
        linkTranslator =  translateWith $ mkTranslationVector (foreignLinks ^. Store.size) linkTrans
    let importedExprs :: [SomeExpr]     = Elem . snd <$> exprTrans
        importedLinks :: [SomeExprLink] = Elem . snd <$> linkTrans

    forM_ importedLinks $ emit . Payload @(Import // AnyExprLink) . (, ElemTranslations exprTranslator linkTranslator, reinitLinks) . unsafeGeneralize
    forM_ importedExprs $ emit . Payload @(Import // AnyExpr)     . (, ElemTranslations exprTranslator linkTranslator, reinitExprs) . unsafeGeneralize

    return exprTranslator

mkTranslationVector :: Int -> [(Int, Int)] -> Vector Int
mkTranslationVector size knownIxes = Vector.fromList reixed where
    reixed        = go size knownIxes
    go 0 _        = []
    go i []       = 0 : go (i - 1) []
    go i l@(e:es) = if (size - i) == fst e then snd e : go (i - 1) es
                                           else 0     : go (i - 1) l

translateWith :: Vector Int -> (forall t. Elem t -> Elem t)
translateWith v = idx %~ Vector.unsafeIndex v
