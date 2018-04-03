{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Inference.StructuralTyping where

import OCI.IR.Combinators
import Luna.IR
import OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import Luna.Prelude as P hiding (cons)
import Luna.Pass.Data.ExprRoots
import Luna.Pass.Data.UniqueNameGen
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Inference.Data.MergeQueue
import Luna.Pass.Inference.Data.Unifications
import Luna.Pass.Resolution.Data.UnresolvedAccs
import Luna.IR.Term.Literal (isInteger)

data StructuralTyping
type instance Abstract StructuralTyping = StructuralTyping
type instance Inputs  Net   StructuralTyping = '[AnyExpr, AnyExprLink]
type instance Outputs Net   StructuralTyping = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer StructuralTyping = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // UserType, AnyExpr // Succs, AnyExpr // Requester, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer StructuralTyping = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // UserType, AnyExpr // Succs, AnyExpr // Requester, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  StructuralTyping = '[ExprRoots, UniqueNameGen, SimplifierQueue, Unifications, UnresolvedAccs, MergeQueue]
type instance Outputs Attr  StructuralTyping = '[UniqueNameGen, SimplifierQueue, Unifications, UnresolvedAccs, MergeQueue]
type instance Inputs  Event StructuralTyping = '[]
type instance Outputs Event StructuralTyping = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves     StructuralTyping = '[]

runStructuralTyping :: (MonadRef m, MonadIO m, MonadPassManager m) => Pass StructuralTyping m
runStructuralTyping = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ (getStructuralType Nothing) roots


getStructuralType :: (MonadRef m, MonadIO m, MonadPassManager m) => Maybe (Expr Draft) -> Expr Draft -> SubPass StructuralTyping m (Expr Monadic, Expr Draft)
getStructuralType knownMonad expr = do
    tp  <- getLayer @Type   expr >>= source
    matchExpr tp $ \case
        Star         -> attachStructuralType knownMonad expr
        Monadic c m  -> do
            chld <- source c
            return (unsafeGeneralize tp, chld)
        _            -> do
            (mon, tp) <- return tp `inMonadM` var =<< genName
            reconnectLayer @Type expr mon
            return (mon, tp)

inMonad :: (MonadPassManager m, MonadRef m) => Expr Draft -> SubPass StructuralTyping m (Expr Draft) -> SubPass StructuralTyping m (Expr Monadic)
inMonad e mm = fmap generalize $ mm >>= monadic e

infixr 1 `inMonadM`
inMonadM :: (MonadPassManager m, MonadRef m, Generalizable' (Expr l) (Expr Draft), Generalizable' (Expr r) (Expr Draft)) => SubPass StructuralTyping m (Expr l) -> SubPass StructuralTyping m (Expr r) -> SubPass StructuralTyping m (Expr Monadic, Expr Draft)
inMonadM me mm = do
    e <- me
    m <- inMonad (generalize e) $ generalize <$> mm
    return (generalize m, generalize e)

attachStructuralType :: (MonadRef m, MonadIO m, MonadPassManager m) => Maybe (Expr Draft) -> Expr Draft -> SubPass StructuralTyping m (Expr Monadic, Expr Draft)
attachStructuralType knownMonad expr = do
    (tp, ch) <- matchExpr expr $ \case
        Number x  -> cons_ @Draft (if isInteger x then "Int" else "Real") `inMonadM` cons_ @Draft "Pure"
        String {} -> cons_ @Draft "Text" `inMonadM` cons_ @Draft "Pure"
        Acc a n   -> do
            (aM, aT) <- getStructuralType Nothing =<< source a
            tacc@(macc, _) <- acc aM n `inMonadM` var =<< genName
            reconnectLayer' @Requester (Just expr) macc
            modifyAttr_ @UnresolvedAccs $ addAcc macc
            return tacc
        App f a -> do
            (aM, aT) <- getStructuralType Nothing =<< source a
            (fM, fT) <- getStructuralType Nothing =<< source f
            tapp@(mapp, _) <- app fM aM `inMonadM` var =<< genName
            reconnectLayer' @Requester (Just expr) mapp
            modifyAttr_ @SimplifierQueue $ wrap . (mapp :) . unwrap
            return tapp
        Lam a o -> do
            freshVar <- fmap generalize $ var =<< genName
            (aM, _) <- getStructuralType (Just freshVar) =<< source a
            (oM, _) <- getStructuralType Nothing         =<< source o
            lam aM oM `inMonadM` cons_ @Draft "Pure"
        Unify a b -> do
            pure <- fmap generalize $ cons_ @Draft "Pure"
            (lM, lT) <- getStructuralType (Just pure) =<< source a
            (rM, rT) <- getStructuralType Nothing     =<< source b
            uT <- unify lT rT
            reconnectLayer' @Requester (Just expr) uT
            modifyAttr_ @Unifications $ wrap . (generalize uT :) . unwrap
            mon <- source =<< (view (wrapped . termMonadic_monad)) <$> readTerm rM
            return uT `inMonadM` return mon
        ASGFunction n as o -> do
            asM <- forM as $ \a -> do
                freshVar <- fmap generalize $ var =<< genName
                (aM, aT) <- getStructuralType (Just freshVar) =<< source a
                return aM
            (oM, oT) <- getStructuralType Nothing =<< source o
            (nM, _)  <- getStructuralType Nothing =<< source n
            let lamsInPure []       b = return b
                lamsInPure (a : as) b = do
                    (restM, _) <- lamsInPure as b
                    lam a restM `inMonadM` cons_ @Draft "Pure"
            res@(resM, _) <- lamsInPure asM (oM, oT)
            uni <- unify nM resM
            reconnectLayer' @Requester (Just expr) (generalize uni :: Expr Draft)
            modifyAttr_ @Unifications $ wrap . (generalize uni :) . unwrap
            return res
        Grouped a  -> getStructuralType knownMonad =<< source a
        Marked _ b -> getStructuralType knownMonad =<< source b
        Seq a b   -> do
            (lM, lT)  <- getStructuralType Nothing =<< source a
            (rM, rT)  <- getStructuralType Nothing =<< source b
            lchild    <- source =<< (view (wrapped . termMonadic_monad) <$> readTerm lM)
            rchild    <- source =<< (view (wrapped . termMonadic_monad) <$> readTerm rM)
            cM        <- unify lchild rchild
            modifyAttr_ @MergeQueue $ wrap . (generalize cM :) . unwrap
            return rT `inMonadM` return cM
        Match a cls -> do
            (iM, iT) <- getStructuralType Nothing =<< source a
            clauses  <- mapM (source >=> getStructuralType Nothing) cls
            newtp    <- var =<< genName
            apps     <- forM clauses $ \(cM, cT) -> do
                aT <- app cM iM `inMonadM` var =<< genName
                modifyAttr_ @SimplifierQueue $ wrap . (fst aT :) . unwrap
                reconnectLayer' @Requester (Just expr) (fst aT)
                return aT
            typeUnis <- mapM (unify newtp . snd) apps
            forM_ typeUnis $ \(generalize -> uni :: Expr Draft) ->
                reconnectLayer' @Requester (Just expr) uni
            modifyAttr_ @Unifications $ wrap . (fmap generalize typeUnis ++) . unwrap
            monUni   <- mergeMany =<< mapM (source <=< fmap (view $ wrapped . termMonadic_monad) . readTerm . fst) apps
            return newtp `inMonadM` return monUni
        _ -> do
            inps <- inputs expr >>= mapM source
            mapM (getStructuralType knownMonad) inps
            tpV <- var =<< genName
            mon <- case knownMonad of
                Just m  -> return m
                Nothing -> fmap generalize $ var =<< genName
            return tpV `inMonadM` return mon
    oldTp  <- getLayer @Type expr >>= source
    reconnectLayer @Type tp expr
    deleteSubtree oldTp
    userTp <- getLayer @UserType expr >>= mapM source
    forM_ userTp $ \utp -> do
        uni <- unify utp tp
        modifyAttr_ @Unifications $ wrap . (generalize uni :) . unwrap
        reconnectLayer' @UserType (Nothing :: Maybe $ Expr Draft) expr
        reconnectLayer  @Type utp expr
    return (tp, ch)

mergeMany :: (MonadRef m, MonadIO m, MonadPassManager m) => [Expr Draft] -> SubPass StructuralTyping m (Expr Draft)
mergeMany [mon] = return mon
mergeMany (mon : rest) = do
    rM   <- mergeMany rest
    uniM <- unify mon rM
    modifyAttr_ @MergeQueue $ wrap . (generalize uniM :) . unwrap
    return $ generalize uniM
