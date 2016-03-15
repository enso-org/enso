{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

{-# LANGUAGE UndecidableInstances #-} -- used for resolution monad, delete after refactoring

module Luna.Compilation.Pass.Inference.Unification where

import Prelude.Luna

import Data.Construction
import Data.Container                               hiding (impossible)
import Data.List                                    (delete)
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Data.Index
import Luna.Syntax.AST.Term                         hiding (source, target)
import Data.Graph.Builder                           hiding (run)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder            (HasSuccs, readSuccs)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Ident.Pool                       (MonadIdentPool, newVarIdent')
import Type.Inference
import Data.Graph.Backend.VectorGraph               as Graph hiding (add, remove)

import qualified Data.Graph.Builder                     as Graph
import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import qualified Luna.Syntax.AST.Term.Lit               as Lit


import Control.Monad.Fix
import Control.Monad (liftM, MonadPlus(..))

import Control.Monad.Trans.Either

#define PassCtx(m,ls,term) ( term ~ Draft Static                            \
                           , ne   ~ Link (ls :<: term)                      \
                           , nodeRef ~ Ref Node (ls :<: term)               \
                           , Prop Type   (ls :<: term) ~ Ref Edge ne        \
                           , HasSuccs (ls :<: term)                         \
                           , BiCastable     e ne                            \
                           , BiCastable     n (ls :<: term)                 \
                           , MonadBuilder (Hetero (VectorGraph n e c)) (m)  \
                           , HasProp Type       (ls :<: term)               \
                           , NodeInferable  (m) (ls :<: term)               \
                           , TermNode Var   (m) (ls :<: term)               \
                           , TermNode Lam   (m) (ls :<: term)               \
                           , TermNode Unify (m) (ls :<: term)               \
                           , TermNode Acc   (m) (ls :<: term)               \
                           , MonadIdentPool (m)                             \
                           , Destructor     (m) (Ref Node (ls :<: term))    \
                           , MonadTypeCheck (ls :<: term) (m)               \
                           )

-------------------------
-- === ResolutionT === --
-------------------------

class Monad m => MonadResolution r m | m -> r where
    resolve :: r -> m ()

newtype ResolutionT r m u = ResolutionT (EitherT r m u) deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)
makeWrapped ''ResolutionT

-- === Utils === --

runResolutionT :: Monad m => ResolutionT r m u -> m (Resolution r u)
runResolutionT m = runEitherT (unwrap' m) >>= return ∘ \case
    Left  l -> Resolved   l
    Right r -> Unresolved r


---- === Instances === --

---- Show
deriving instance Show (Unwrapped (ResolutionT r m u)) => Show (ResolutionT r m u)

---- MonadResolution

instance Monad m => MonadResolution r (ResolutionT r m) where
    resolve = wrap' ∘ left
    {-# INLINE resolve #-}

data Resolution r u = Resolved   r
                    | Unresolved u
                    deriving (Show)





resolve_ = resolve []

resolveUnify :: forall m ls term nodeRef ne ter n e c. (PassCtx(m,ls,term),
                MonadResolution [nodeRef] m)
             => nodeRef -> m ()
resolveUnify uni = do
    uni' <- read uni
    caseTest (uncover uni') $ do
        of' $ \(Unify lc rc) -> do
            l  <- follow source lc
            r  <- follow source rc

            resolveReflexivity uni l r
            symmetrical (resolveStar uni) l r
            symmetrical (resolveVar  uni) l r
            resolveCons uni l r
            resolveLams uni l r

        of' $ \ANY -> impossible

    where symmetrical f a b = f a b *> f b a

          resolveReflexivity uni a b = do
              if a == b
                  then do
                      replaceNode uni a
                      resolve_
                  else return ()

          resolveCons uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              whenMatched (uncover a') $ \(Cons na argsA) ->
                  whenMatched (uncover b') $ \(Cons nb argsB) ->
                      if na == nb && length argsA == length argsB
                          then do
                              asA <- mapM (follow source . unlayer) argsA
                              asB <- mapM (follow source . unlayer) argsB
                              newUnis <- zipWithM unify asA asB
                              unified <- replaceAny a b
                              replaceNode uni unified
                              resolve newUnis
                          else return ()

          resolveStar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \Lit.Star -> do
                  replaceNode uni b
                  resolve_

          resolveVar uni a b = do
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \(Var v) -> do
                  replaceNode uni b
                  replaceNode a   b
                  resolve_

          resolveLams uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              whenMatched (uncover a') $ \(Lam cargs cout) ->
                  whenMatched (uncover b') $ \(Lam cargs' cout') -> do
                    let cRawArgs  = unlayer <$> cargs
                    let cRawArgs' = unlayer <$> cargs'
                    args  <- mapM (follow source) (cout  : cRawArgs )
                    args' <- mapM (follow source) (cout' : cRawArgs')
                    if length args == length args'
                        then do
                            unis  <- zipWithM unify args args'
                            replaceNode uni a
                            replaceNode b   a
                            resolve unis
                        else return ()


replaceAny :: forall m ls term nodeRef ne ter n e c. PassCtx(m,ls,term) => nodeRef -> nodeRef -> m nodeRef
replaceAny r1 r2 = do
    n1 <- read r1
    n2 <- read r2
    if size (n1 # Succs) > size (n2 # Succs)
        then replaceNode r2 r1 >> return r1
        else replaceNode r1 r2 >> return r2

replaceNode oldRef newRef = do
    oldNode <- read oldRef
    forM (readSuccs oldNode) $ \e -> do
        withRef e      $ source     .~ newRef
        withRef newRef $ prop Succs %~ add (unwrap e)
    destruct oldRef

whenMatched a f = caseTest a $ do
    of' f
    of' $ \ANY -> return ()


data TCStatus = TCStatus { _terms     :: Int
                         , _coercions :: Int
                         } deriving (Show)

makeLenses ''TCStatus

-- FIXME[WD]: we should not return [Graph n e] from pass - we should use ~ IterativePassRunner instead which will handle iterations by itself
run :: forall nodeRef m ls term n e ne c.
       ( PassCtx(ResolutionT [nodeRef] m,ls,term)
       , MonadBuilder (Hetero (VectorGraph n e c)) m
       ) => [nodeRef] -> m [Resolution [nodeRef] nodeRef]
run unis = forM unis $ \u -> fmap (resolveUnifyY u) $ runResolutionT $ resolveUnify u


universe = Ref 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?

---- FIXME[WD]: Change the implementation to list builder
--resolveUnifyX :: (PassCtx(ResolutionT [nodeRef] m,ls,term), nodeRef ~ Ref (Node $ (ls :<: term)), MonadIO m, Show (ls :<: term))
--              => nodeRef -> m [nodeRef]
--resolveUnifyX uni = (runResolutionT ∘ resolveUnify) uni >>= return ∘ \case
--    Resolved unis -> unis
--    Unresolved _  -> [uni]

resolveUnifyY uni = \case
    Resolved unis -> Resolved   unis
    Unresolved _  -> Unresolved uni


catUnresolved [] = []
catUnresolved (a : as) = ($ (catUnresolved as)) $ case a of
    Resolved   _ -> id
    Unresolved u -> (u :)

catResolved [] = []
catResolved (a : as) = ($ (catResolved as)) $ case a of
    Unresolved _ -> id
    Resolved   r -> (r :)


--------------------------
-- !!!!!!!!!!!!!!!!!!!! --
--------------------------

-- User - nody i inputy do funkcji bedace varami sa teraz zjadane, moze warto dac im specjalny typ?
-- pogadac z Marcinem o tym

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data UnificationPass = UnificationPass deriving (Show, Eq)

instance ( PassCtx(ResolutionT [nodeRef] m,ls,term)
         , MonadBuilder (Hetero (VectorGraph n e c)) m
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass UnificationPass m where
    hasJobs _ = not . null . view TypeCheck.unresolvedUnis <$> TypeCheck.get

    runTCPass _ = do
        unis <- view TypeCheck.unresolvedUnis <$> TypeCheck.get
        results <- run unis
        let newUnis = catUnresolved results ++ (concat $ catResolved results)
        TypeCheck.modify_ $ TypeCheck.unresolvedUnis .~ newUnis
        case catResolved results of
            [] -> return Stuck
            _  -> return Progressed



