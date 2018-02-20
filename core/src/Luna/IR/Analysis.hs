module Luna.IR.Analysis where

import Luna.Prelude

import Luna.IR
import qualified Data.Set as Set
import           Data.Set (Set)
import Control.Monad.State.Dependent



---------------------------------
-- === Isomorphism checker === --
---------------------------------

-- === Definition === --

type CheckerT           m = StateT ExprIsoChecker (MaybeT m)
type IsomorphicCheckCtx m = (IsomorphicCheckReq m, MonadRef m)
type IsomorphicCheckReq m = Req m '[ Reader // Net   // '[AnyExpr, AnyExprLink]
                                   , Reader // Layer // AnyExpr     // Model
                                   , Reader // Layer // AnyExprLink // Model
                                   ]

data ExprIsoChecker = ExprIsoChecker { _isoPairs :: Set (Expr Draft, Expr Draft)
                                     , _isoElsL  :: Set (Expr Draft)
                                     , _isoElsR  :: Set (Expr Draft)
                                     } deriving (Show)
makeLenses ''ExprIsoChecker



-- === Utils === --

checkIsoExpr        :: IsomorphicCheckCtx m =>            Expr Draft  ->            Expr Draft  -> m Bool
checkIsoRootedExpr  :: IsomorphicCheckCtx m => Rooted    (Expr Draft) -> Rooted    (Expr Draft) -> m Bool
checkIsoRootedMExpr :: IsomorphicCheckCtx m => RootedM m (Expr Draft) -> RootedM m (Expr Draft) -> m Bool
checkIsoExpr        a b = join $ checkIsoRootedMExpr <$> rootedST   a <*> rootedST   b
checkIsoRootedExpr  a b = join $ checkIsoRootedMExpr <$> unsafeThaw a <*> unsafeThaw b
checkIsoRootedMExpr a b = isJustT $ evalDefStateT @ExprIsoChecker $ maybeIsoRootedMExpr a b

checkIsoLinks     :: IsomorphicCheckCtx m =>   RootedM m (ExprLink' Draft) -> RootedM m (ExprLink' Draft)   -> CheckerT m ()
checkManyIsoLinks :: IsomorphicCheckCtx m => [(RootedM m (ExprLink' Draft),   RootedM m (ExprLink' Draft))] -> CheckerT m ()
checkIsoLinks le re = join  $ maybeIsoRootedMExpr <$> mapM readSource le <*> mapM readSource re
checkManyIsoLinks   = mapM_ $ uncurry checkIsoLinks

assumeIsomorphism :: Monad m => Expr Draft -> Expr Draft -> CheckerT m ()
assumeIsomorphism l r = do
    s <- get @ExprIsoChecker
    let unchecked = Set.notMember l (s ^. isoElsL) && Set.notMember r (s ^. isoElsR)
    if  unchecked then modify_ @ExprIsoChecker (isoPairs %~ Set.insert (l, r)) else mzero

checkIsoPair :: MonadGetter ExprIsoChecker m => Expr Draft -> Expr Draft -> m Bool
checkIsoPair l r = Set.member (l, r) . view isoPairs <$> get @ExprIsoChecker

matchBoth :: (MonadRef m, Reader Layer (AnyExpr // Model) m, uniTerm ~ Unwrapped (UniTerm' (Expr layout)))
          => RootedM m (Expr layout) -> RootedM m (Expr layout) -> (uniTerm -> uniTerm -> CheckerT m ()) -> CheckerT m ()
matchBoth e1 e2 l = matchRootedMExpr e2 =<< matchRootedMExpr e1 (return . l) where
    matchRootedMExpr (RootedST irm expr) f = with @IRST irm $ matchExpr expr f

maybeIsoRootedMExpr :: IsomorphicCheckCtx m => RootedM m (Expr Draft) -> RootedM m (Expr Draft) -> CheckerT m ()
maybeIsoRootedMExpr  l r = whenM (not <$> checkIsoPair (l ^. root) (r ^. root)) $ do
    assumeIsomorphism (l ^. root) (r ^. root)
    matchBoth l r $ \x y -> let
        lrooted a = l & root .~ a
        rrooted a = r & root .~ a
        in case (x, y) of
        (Number      a, Number  a'   ) -> guard (a == a')
        (String      a, String  a'   ) -> guard (a == a')
        (Var       v  , Var     v'   ) -> guard (v == v')
        (Acc       a n, Acc     a' n') -> guard (n == n') >> checkIsoLinks     (lrooted a) (rrooted a')
        (Cons      c s, Cons    c' s') -> guard (c == c') >> checkManyIsoLinks (zip (lrooted <$> s) (rrooted <$> s'))
        (App       f a, App     f' a') -> checkManyIsoLinks [(lrooted f, rrooted f'), (lrooted a, rrooted a')]
        (Unify     l r, Unify   l' r') -> checkManyIsoLinks [(lrooted l, rrooted l'), (lrooted r, rrooted r')]
        (Lam       a b, Lam     a' b') -> checkManyIsoLinks [(lrooted a, rrooted a'), (lrooted b, rrooted b')]
        (Grouped   a  , Grouped a'   ) -> checkIsoLinks (lrooted a) (rrooted a')
        (Blank        , Blank        ) -> return ()
        (Missing      , Missing      ) -> return ()
        (Star         , Star         ) -> return ()
        (_, _)                         -> mzero


-- === Instances === --

instance Mempty  ExprIsoChecker where mempty = def
instance Default ExprIsoChecker where
    def = ExprIsoChecker def def def
