{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Inference.UnificationSolver where

import Data.Ratio   (denominator)
import Luna.Prelude as P hiding (cons)
import Luna.IR
import OCI.IR.Combinators
import System.Log
import Control.Monad.Raise
import Control.Monad.State.Dependent (MonadGetter, MonadSetter)
import Data.Maybe (isJust)
import OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.IR.Expr as Term
import OCI.IR.Term (Term(Term))
import Luna.Pass.Inference.Data.Unifications
import qualified Luna.IR.Term.Literal as Number

data UnificationSolver
type instance Abstract      UnificationSolver = UnificationSolver
type instance Inputs  Net   UnificationSolver = '[AnyExpr, AnyExprLink]
type instance Outputs Net   UnificationSolver = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer UnificationSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Requester, AnyExpr // Errors, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer UnificationSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Requester, AnyExpr // Errors, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  UnificationSolver = '[Unifications]
type instance Outputs Attr  UnificationSolver = '[Unifications]
type instance Inputs  Event UnificationSolver = '[]
type instance Outputs Event UnificationSolver = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves     UnificationSolver = '[]

type SubPassReq k t m = (Readers k (Inputs k t) m, Writers k (Outputs k t) m)

type MonadSubPass t m = (SubPassReq Net t m, SubPassReq Layer t m, SubPassReq Attr t m, Emitters (Outputs Event t) m)

class Monad m => MonadResolution r m | m -> r where
    resolve :: r -> m ()

resolve_ :: MonadResolution [t] m => m ()
resolve_ = resolve []

-------------------------
-- === ResolutionT === --
-------------------------

-- === Definition === --

newtype ResolutionT r m u = ResolutionT (EitherT r m u) deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans, MonadLogging)
makeWrapped ''ResolutionT


-- === Utils === --

runResolutionT :: Monad m => ResolutionT r m u -> m (Either r u)
runResolutionT = runEitherT . unwrap'


-- === Instances === --

-- State
instance MonadGetter s m => MonadGetter s (ResolutionT r m)
instance MonadSetter s m => MonadSetter s (ResolutionT r m)

instance Monad m => MonadResolution r (ResolutionT r m) where
    resolve = wrap' . left

instance PrimMonad m => PrimMonad (ResolutionT r m) where
    type PrimState (ResolutionT r m) = PrimState m
    primitive = lift . primitive

type instance GetRefHandler (ResolutionT r m) = GetRefHandler m

instance Emitter a m => Emitter a (ResolutionT r m)


-- FIXME[WD -> MK]: this should be generalized, however I'm pretty sure there is some standard function that does exactly this
symmetrical :: Applicative f => (a -> a -> f b) -> a -> a -> f b
symmetrical f a b = f a b *> f b a

repType :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m) => Expr Draft -> m Text
repType expr = matchExpr expr $ \case
    Var n   -> return $ "(" <> convert n <> ")"
    Acc t n -> do
        trep <- repType =<< source t
        return $ "(" <> trep <> ")." <> convert n
    Lam i o -> do
        irep <- repType =<< source i
        orep <- repType =<< source o
        return $ irep <> " -> (" <> orep <> ")"
    App f a -> do
        frep <- repType =<< source f
        arep <- repType =<< source a
        return $ "(" <> frep <> ") (" <> arep <> ")"
    Cons n as -> do
        args <- mapM (repType <=< source) as
        return $ "(" <> convert n <> foldl (<>) "" args <> ")"
    n -> return $ convert $ show n




reportError :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
reportError uni a b = do
    req <- getLayer @Requester uni >>= mapM source
    arep <- repType a
    brep <- repType b
    forM_ req $ \requester -> modifyLayer_ @Errors requester (CompileError ("Unification error: " <> arep <> " with " <> brep) [] :)
    resolve_

reflexivity :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
reflexivity uni a b = when (a == b) $ do
    deleteWithoutInputs uni
    resolve_

variable :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
variable uni a b = do
    isVar <- isJust <$> narrowTerm @Var a
    let go = replace b `mapM` [a, uni] >> resolve_
    when isVar $ matchExpr b $ \case
        Var     {} -> go
        Cons    {} -> go
        Lam     {} -> go
        Monadic {} -> go
        Unify   {} -> go -- This is just for Merge semantics. Merge should not be a Unify at some point, so this will disappear
        _       -> return ()

num :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
num uni a b = match2 a b $ \x y -> case (x, y) of
    (Number n, Cons name _) -> do
        case (name, Number.isInteger n) of
            ("Int", True) -> do
                substitute b a
                deleteWithoutInputs uni
                deleteSubtree a
                resolve_
            _ -> return ()
    _ -> return ()

lambda :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
lambda uni a b = match2 a b $ \x y -> case (x, y) of
    (Lam arg1 out1, Lam arg2 out2) -> do
        uniA <- join $ unify <$> source arg1 <*> source arg2
        uniO <- join $ unify <$> source out1 <*> source out2
        deleteWithoutInputs uni
        resolve [generalize uniA, generalize uniO]
    (Lam _ _, _) -> reportError uni a b
    (_, Lam _ _) -> reportError uni a b
    _ -> return ()

conses :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
conses uni a b = match2 a b $ \x y -> case (x, y) of
    (Cons n1 fs1, Cons n2 fs2) -> do
        if (n1 == n2) then do
            ins1 <- mapM source fs1
            ins2 <- mapM source fs2
            unis <- zipWithM unify ins1 ins2
            deleteSubtree uni
            resolve $ generalize <$> unis
        else reportError uni a b
    _ -> return ()

monads :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
monads uni a b = match2 a b $ curry $ \case
    (Monadic t1' m1', Monadic t2' m2') -> do
        t1 <- source t1'
        t2 <- source t2'
        m1 <- source m1'
        m2 <- source m2'
        ut <- unify t1 t2
        um <- unify m1 m2
        deleteSubtree uni
        resolve [generalize ut, generalize um]
    _ -> return ()

solveUnification :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m) => Expr Unify -> m (Either [Expr Unify] ())
solveUnification uni = do
    req <- getLayer @Requester uni >>= mapM source
    solution <- runResolutionT $ do
        let uni' = generalize uni
        Term (Term.Unify l' r') <- readTerm uni
        l <- source l'
        r <- source r'
        reflexivity uni' l r
        symmetrical (variable uni') l r
        symmetrical (num      uni') l r
        lambda uni' l r
        conses uni' l r
        monads uni' l r
    case solution of
        Left new -> mapM_ (reconnectLayer' @Requester req) new
        Right _  -> return ()
    return solution


deepSolve :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m) => Expr Unify -> m [Expr Unify]
deepSolve uni = do
    res <- solveUnification uni
    case res of
        Left new -> concat <$> mapM deepSolve new
        Right _  -> return [uni]


runUnificationSolver :: (MonadRef m, MonadPassManager m) => SubPass UnificationSolver m Bool
runUnificationSolver = do
    unis        <- unwrap <$> getAttr @Unifications
    outstanding <- concat <$> mapM deepSolve unis
    putAttr @Unifications $ wrap outstanding
    return $ unis /= outstanding
