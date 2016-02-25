{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

{-# LANGUAGE UndecidableInstances #-} -- used for resolution monad, delete after refactoring

module Luna.Compilation.Pass.Inference.Unification where

import Prelude.Luna

import Data.Graph
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
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                  (MonadIdentPool, newVarIdent')
import Type.Inference

import qualified Data.Graph.Builder                     as Graph
import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck
import qualified Luna.Syntax.Name                       as Name
import Data.Graph.Backend.VectorGraph                   as Graph



import Control.Monad.Fix
import Control.Monad (liftM, MonadPlus(..))

import Control.Monad.Trans.Either

#define PassCtx(m,ls,term) ( term ~ Draft Static                            \
                           , ne   ~ Link (ls :<: term)                      \
                           , nodeRef ~ Ref Node (ls :<: term)               \
                           , Prop Type   (ls :<: term) ~ Ref Edge ne        \
                           , Prop Succs  (ls :<: term) ~ [Ref Edge ne]      \
                           , BiCastable     e ne                            \
                           , BiCastable     n (ls :<: term)                 \
                           , MonadBuilder (Hetero (VectorGraph n e c)) (m)  \
                           , HasProp Type       (ls :<: term)               \
                           , HasProp Succs      (ls :<: term)               \
                           , NodeInferable  (m) (ls :<: term)               \
                           , TermNode Var   (m) (ls :<: term)               \
                           , TermNode Lam   (m) (ls :<: term)               \
                           , TermNode Unify (m) (ls :<: term)               \
                           , TermNode Acc   (m) (ls :<: term)               \
                           , TermNode Sub   (m) (ls :<: term)               \
                           , MonadIdentPool (m)                             \
                           , Destructor     (m) (Ref Node (ls :<: term))    \
                           , MonadTypeCheck (ls :<: term) (m)               \
                           , MonadIO (m) \
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
                    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)





resolve_ = resolve []

resolveUnify :: forall m ls term nodeRef ne ter n e c. (PassCtx(m,ls,term),
                MonadResolution [nodeRef] m)
             => nodeRef -> m ()
resolveUnify uni = do
    uni' <- read uni
    caseTest (uncover uni') $ do
        match $ \(Unify lc rc) -> do
            l  <- follow source lc
            r  <- follow source rc

            symmetrical (resolveStar uni) l r
            symmetrical (resolveVar  uni) l r
            --symmetrical (resolveAcc  uni) l r
            resolveConses uni l r
            resolveLams   uni l r

        match $ \ANY -> impossible

    where symmetrical f a b = f a b *> f b a

          resolveStar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \Star -> do
                  replaceNode uni b
                  resolve_

          resolveVar uni a b = do
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \(Var _) -> do
                  replaceNode uni b
                  replaceNode a   b
                  resolve_

          --                        -- THIS IS WRONG! we cannot unify Acc with anything!
          --resolveAcc uni a b = do -- treat Acc as a Var: the importer will take care of this later on
          --                        -- FIXME[WD->MK]: The above comment cannot be understand without context
          --    a'   <- read (a :: nodeRef)
          --    whenMatched (uncover a') $ \(Acc _ _) -> do
          --        replaceNode uni b
          --        replaceNode a   b
          --        resolve_

          resolveConses uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              whenMatched (uncover a') $ \(Cons an) ->
                  whenMatched (uncover b') $ \(Cons bn) -> if an == bn
                      then do
                          replaceNode uni b
                          replaceNode a   b
                          resolve_
                      else return () -- FIXME[WD->MK]: should report error!

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
                    unis  <- zipWithM unify args args'

                    replaceNode uni a
                    replaceNode b   a
                    resolve unis


resolveSub :: forall m ls term nodeRef ne ter n e c. (PassCtx(m,ls,term),
                MonadResolution [nodeRef] m)
             => nodeRef -> m ()
resolveSub sb = do
    sb' <- read sb
    caseTest (uncover sb') $ do
        match $ \(Sub srcRef tgtRef) -> do
            src <- follow source srcRef
            tgt <- follow source tgtRef

            --resolveVar    sb tgt src
            --resolveConses sb src tgt
            --resolveLams   sb src tgt
            return ()

        match $ \ANY -> impossible

    where symmetrical f a b = f a b *> f b a

          --resolveConses s a b = do
          --    s' <- read s
          --    a' <- read (a :: nodeRef)
          --    b' <- read (b :: nodeRef)
          --    whenMatched (uncover a') $ \(Cons an) ->
          --        whenMatched (uncover b') $ \(Cons bn) -> if an == bn
          --            then do
          --                replaceNode s b
          --                replaceNode a b
          --                resolve_
          --            else return () -- FIXME[WD->MK]: should report error!

          --resolveLams s a b = do
          --    s' <- read s
          --    a' <- read (a :: nodeRef)
          --    b' <- read (b :: nodeRef)
          --    whenMatched (uncover a') $ \(Lam cargs cout) ->
          --        whenMatched (uncover b') $ \(Lam cargs' cout') -> do
          --          let cRawArgs  = unlayer <$> cargs
          --          let cRawArgs' = unlayer <$> cargs'
          --          args  <- mapM (follow source) (cout  : cRawArgs )
          --          args' <- mapM (follow source) (cout' : cRawArgs')
          --          subs  <- zipWithM sub args args'

          --          replaceNode s a
          --          replaceNode b a
          --          resolve subs

          --resolveVar uni a b = do
          --    a'   <- read (a :: nodeRef)
          --    whenMatched (uncover a') $ \(Var _) -> do
          --        replaceNode uni b
          --        replaceNode a   b
          --        resolve_


replaceNode oldRef newRef = do
    oldNode <- read oldRef
    forM (oldNode ^. prop Succs) $ \e -> do
        withRef e      $ source     .~ newRef
        withRef newRef $ prop Succs %~ (e :)
    destruct oldRef

whenMatched a f = caseTest a $ do
    match f
    match $ \ANY -> return ()


data TCStatus = TCStatus { _terms     :: Int
                         , _coercions :: Int
                         } deriving (Show)

makeLenses ''TCStatus

-- CHECKME[WD]: for now this pass handles both unifications and substitutions. If we will be sure that they don't affect each other, we should partition it to 2 passes
--              in order to slightly improve performance (sometimes this pass would be called after only substitutions updates)
run :: forall nodeRef m ls term n e ne c.
       ( PassCtx(ResolutionT [nodeRef] m,ls,term)
       , MonadBuilder (Hetero (VectorGraph n e c)) m, MonadIO m
       ) => [nodeRef] -> [nodeRef] -> m ([Resolution [nodeRef] nodeRef], [Resolution [nodeRef] nodeRef])
run unis subs = do
    uniRes <- forM unis $ \u -> fmap (fmap $ const u) $ runResolutionT $ resolveUnify u
    subRes <- forM subs $ \s -> fmap (fmap $ const s) $ runResolutionT $ resolveSub   s
    return (uniRes, subRes)


universe = Ref 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?



catUnresolved [] = []
catUnresolved (a : as) = ($ (catUnresolved as)) $ case a of
    Resolved   _ -> id
    Unresolved u -> (u :)

catResolved [] = []
catResolved (a : as) = ($ (catResolved as)) $ case a of
    Unresolved _ -> id
    Resolved   r -> (r :)



-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data UnificationPass = UnificationPass deriving (Show, Eq)

instance ( PassCtx(ResolutionT [nodeRef] m,ls,term)
         , MonadBuilder (Hetero (VectorGraph n e c)) m
         , MonadTypeCheck (ls :<: term) m
         , MonadIO m
         ) => TypeCheckerPass UnificationPass m where
    hasJobs _ = do
        t1 <- not . null . view TypeCheck.unresolvedUnis <$> TypeCheck.get
        t2 <- not . null . view TypeCheck.unresolvedSubs <$> TypeCheck.get
        return $ t1 || t2



    runTCPass _ = do
        unis <- view TypeCheck.unresolvedUnis <$> TypeCheck.get
        subs <- view TypeCheck.unresolvedSubs <$> TypeCheck.get
        (uniRes, subRes) <- run unis subs
        let newUnis = concat $ catResolved uniRes
            newSubs = concat $ catResolved subRes
            unis' = catUnresolved uniRes ++ newUnis
            subs' = catUnresolved subRes ++ newSubs
        TypeCheck.modify_ $ (TypeCheck.unresolvedUnis .~ unis')
                          . (TypeCheck.unresolvedSubs .~ subs')
        return $ if (null newUnis && null newSubs) then Stuck else Progressed


