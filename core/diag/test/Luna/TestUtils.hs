module Luna.TestUtils where

import           Luna.Prelude
import           Luna.IR
import           Luna.IR.Function.Argument (Arg (..))
import           Control.Monad.Trans.Maybe
import           Control.Monad.State       as State   hiding (when)
import           Control.Monad             (guard)
import           Data.Maybe                (isJust)
import           Test.Hspec                (expectationFailure, Expectation)
import qualified Data.Set                  as Set
import           Data.Set                  (Set)

---------------------------
-- === HSpec helpers === --
---------------------------

withRight :: Show (Either a b) => Either a b -> (b -> Expectation) -> Expectation
withRight e exp = either (const $ expectationFailure $ "Expected a Right, got: (" <> show e <> ")") exp e

-------------------------------
-- === Isomorphism check === --
-------------------------------

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

-----------------------------
-- === Coherence check === --
-----------------------------

data IncoherenceType = DanglingSource
                     | DanglingTarget
                     | OrphanedSuccessor
                     | OrphanedInput
                     deriving (Show, Eq)

data Incoherence = Incoherence IncoherenceType AnyExpr AnyExprLink deriving (Show, Eq)

data CoherenceCheck = CoherenceCheck { _incoherences :: [Incoherence]
                                     , _allExprs     :: [AnyExpr]
                                     , _allLinks     :: [AnyExprLink]
                                     } deriving (Show)
makeLenses ''CoherenceCheck

type MonadCoherenceCheck m = (MonadState CoherenceCheck m, CoherenceCheckCtx m)
type CoherenceCheckCtx   m = (IRMonad m, Readables m '[ExprLayer Model, ExprLayer Type, ExprLayer Succs, ExprLinkLayer Model, ExprNet, ExprLinkNet])

checkCoherence :: CoherenceCheckCtx m => m [Incoherence]
checkCoherence = do
    es <- exprs
    ls <- links
    s <- flip execStateT (CoherenceCheck def es ls) $ do
        mapM_ checkExprCoherence es
        mapM_ checkLinkCoherence ls
    return $ s ^. incoherences

reportIncoherence :: MonadCoherenceCheck m => Incoherence -> m ()
reportIncoherence i = State.modify (incoherences %~ (i:))

checkExprCoherence :: MonadCoherenceCheck m => AnyExpr -> m ()
checkExprCoherence e = checkInputs e >> checkSuccs e

checkInputs :: MonadCoherenceCheck m => AnyExpr -> m ()
checkInputs e = do
    tp     <- readLayer @Type e
    fields <- symbolFields e
    links  <- use allLinks
    forM_ (tp : fields) $ \l -> do
        (_, tgt) <- readLayer @Model l
        when (tgt /= e || not (elem l links)) $ reportIncoherence $ Incoherence OrphanedInput e l

checkSuccs :: MonadCoherenceCheck m => AnyExpr -> m ()
checkSuccs e = do
    succs <- readLayer @Succs e
    links <- use allLinks
    forM_ (Set.toList succs) $ \l -> do
        (src, _) <- readLayer @Model l
        when (src /= e || not (elem l links)) $ reportIncoherence $ Incoherence OrphanedSuccessor e l

checkLinkCoherence :: MonadCoherenceCheck m => AnyExprLink -> m ()
checkLinkCoherence l = do
    (src, tgt) <- readLayer @Model l
    exprs <- use allExprs
    when (not $ elem src exprs) $ reportIncoherence $ Incoherence DanglingSource src l
    when (not $ elem tgt exprs) $ reportIncoherence $ Incoherence DanglingTarget tgt l
    checkIsSuccessor l src
    checkIsInput     l tgt

checkIsSuccessor :: MonadCoherenceCheck m => AnyExprLink -> AnyExpr -> m ()
checkIsSuccessor l e = do
    succs <- readLayer @Succs e
    when (Set.notMember l succs) $ reportIncoherence $ Incoherence DanglingSource e l

checkIsInput :: MonadCoherenceCheck m => AnyExprLink -> AnyExpr -> m ()
checkIsInput l e = do
    tp <- readLayer @Type e
    fs <- symbolFields e
    when (tp /= l && not (elem l fs)) $ reportIncoherence $ Incoherence DanglingTarget e l
