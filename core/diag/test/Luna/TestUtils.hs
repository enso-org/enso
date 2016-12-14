module Luna.TestUtils where

import           Luna.Prelude
import           Luna.IR
import           Luna.IR.Function.Argument (Arg (..))
import           Control.Monad.Trans.Maybe
import           Control.Monad (guard)
import           Data.Maybe (isJust)

match2 :: (IRMonad m, Readable (Layer EXPR Model) m, uniTerm ~ Unwrapped (ExprUniTerm (Expr layout)))
       => Expr layout -> Expr layout -> (uniTerm -> uniTerm -> m a) -> m a
match2 e1 e2 l = do
    firstMatched <- match e1 $ return . l
    match e2 firstMatched


newtype ExprIsomorphism l = ExprIsomorphism [(Expr l, Expr l)]
makeWrapped ''ExprIsomorphism

instance Default (ExprIsomorphism l) where
    def = wrap []

areIsomorphic :: Expr l -> Expr l -> ExprIsomorphism l -> Bool
areIsomorphic e1 e2 iso = (e1, e2) `Luna.Prelude.elem` unwrap iso

assumeIsomorphism :: Expr l -> Expr l -> ExprIsomorphism l -> Maybe (ExprIsomorphism l)
assumeIsomorphism e1 e2 (ExprIsomorphism lst) = if nowhere e1 lst && nowhere e2 lst then Just . ExprIsomorphism . ((e1, e2) :) $ lst else Nothing where
    anyEq   e = uncurry (||) . over both (== e)
    nowhere e = all (not . anyEq e)

enrichIsomorphism :: Expr l -> Expr l -> ExprIsomorphism l -> Maybe (ExprIsomorphism l)
enrichIsomorphism e1 e2 iso = if areIsomorphic e1 e2 iso then Just iso else assumeIsomorphism e1 e2 iso

targetsIsomorphic :: (IRMonad m, Readables m ('[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]))
                  => ExprIsomorphism Draft -> Link' (Expr Draft) -> Link' (Expr Draft) -> MaybeT m (ExprIsomorphism Draft)
targetsIsomorphic iso le re = do
    l <- source le
    r <- source re
    exprsIsomorphic iso l r

exprsIsomorphic :: (IRMonad m, Readables m ('[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]))
                => ExprIsomorphism Draft -> Expr Draft -> Expr Draft -> MaybeT m (ExprIsomorphism Draft)
exprsIsomorphic iso l r = if areIsomorphic l r iso then return iso else exprsIsomorphic' iso l r

exprsIsomorphic' :: (IRMonad m, Readables m ('[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]))
                 => ExprIsomorphism Draft -> Expr Draft -> Expr Draft -> MaybeT m (ExprIsomorphism Draft)
exprsIsomorphic' iso l r = do
    assumption <- MaybeT $ return $ assumeIsomorphism l r iso
    match2 l r $ \x y -> case (x, y) of
        (Integer    a, Integer    b) -> guard (a == b) >> return assumption
        (Rational   a, Rational   b) -> guard (a == b) >> return assumption
        (String     a, String     b) -> guard (a == b) >> return assumption
        (Acc      n a, Acc      m b) -> do
            n' <- generalize <$> source n
            m' <- generalize <$> source m
            isoWithNames <- exprsIsomorphic assumption n' m'
            targetsIsomorphic isoWithNames a b
        (App f1 (Arg _ a1), App f2 (Arg _ a2)) -> do
            isoFuns <- targetsIsomorphic assumption f1 f2
            targetsIsomorphic isoFuns a1 a2
        (Unify l1 r1, Unify l2 r2) -> do
            isoLeft <- targetsIsomorphic assumption l1 l2
            targetsIsomorphic isoLeft r1 r2
        (Blank, Blank)   -> return assumption
        (Cons n, Cons m) -> do
            n' <- generalize <$> source n
            m' <- generalize <$> source m
            exprsIsomorphic assumption n' m'
        (Grouped a, Grouped b) -> targetsIsomorphic assumption a b
        (Lam (Arg _ a1) o1, Lam (Arg _ a2) o2) -> do
            isoArgs <- targetsIsomorphic assumption a1 a2
            targetsIsomorphic isoArgs o1 o2
        (Missing, Missing) -> return assumption
        (Star, Star) -> return assumption
        (Var n, Var m) -> do
            n' <- generalize <$> source n
            m' <- generalize <$> source m
            exprsIsomorphic assumption n' m'
        (_, _) -> mzero

areExpressionsIsomorphic :: (IRMonad m, Readables m ('[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]))
                         => Expr Draft -> Expr Draft -> m Bool
areExpressionsIsomorphic l r = do
    iso <- runMaybeT $ exprsIsomorphic def l r
    return $ isJust iso
