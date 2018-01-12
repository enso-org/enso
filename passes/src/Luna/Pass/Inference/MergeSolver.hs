{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Inference.MergeSolver where

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
import Luna.Pass.Inference.Data.MergeQueue
import qualified Luna.IR.Term.Literal as Number

data MergeSolver
type instance Abstract      MergeSolver = MergeSolver
type instance Inputs  Net   MergeSolver = '[AnyExpr, AnyExprLink]
type instance Outputs Net   MergeSolver = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer MergeSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer MergeSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  MergeSolver = '[MergeQueue]
type instance Outputs Attr  MergeSolver = '[MergeQueue]
type instance Inputs  Event MergeSolver = '[]
type instance Outputs Event MergeSolver = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves     MergeSolver = '[]

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

symmetrical :: Applicative f => (a -> a -> f b) -> a -> a -> f b
symmetrical f a b = f a b *> f b a

reflexivity :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
reflexivity uni a b = when (a == b) $ do
    replace a uni
    resolve_

trivialSum :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
trivialSum uni a b = when (uni == a) $ do
    replace b uni
    resolve_


bothTrivial :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
bothTrivial uni a b = when (uni == a && uni == b) $ do
    c <- cons_ @Draft "Pure"
    replace c uni
    resolve_

conses :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
conses uni a b = match2 a b $ \x y -> case (x, y) of
    (Cons n1 fs1, Cons n2 fs2) -> when (n1 == n2) $ replace a uni >> resolve_
    _                          -> return ()

withPure :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m, MonadResolution [Expr Unify] m) => Expr Draft -> Expr Draft -> Expr Draft -> m ()
withPure uni a b = matchExpr a $ \case
    Cons n1 _ -> when (n1 == "Pure") $ replace b uni >> resolve_
    _         -> return ()

solveMerge :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m) => Expr Unify -> m (Either [Expr Unify] ())
solveMerge uni = runResolutionT $ do
    let uni' = generalize uni
    Term (Term.Unify l' r') <- readTerm uni
    l <- source l'
    r <- source r'
    bothTrivial uni' l r
    reflexivity uni' l r
    symmetrical (trivialSum uni') l r
    symmetrical (withPure   uni') l r
    conses uni' l r

solve :: (MonadRef m, MonadPassManager m, MonadSubPass MergeSolver m) => Expr Unify -> m [Expr Unify]
solve uni = do
    res <- solveMerge uni
    case res of
        Left new -> return []
        Right _  -> return [uni]

runMergeSolver :: (MonadRef m, MonadPassManager m) => Pass MergeSolver m
runMergeSolver = do
    unis        <- unwrap <$> getAttr @MergeQueue
    outstanding <- concat <$> mapM solve unis
    putAttr @MergeQueue $ wrap outstanding
    if length outstanding < length unis && length outstanding /= 0 then runMergeSolver else return ()
