{-# LANGUAGE PartialTypeSignatures #-}

module Luna.Pass.Transform.Desugaring.BlankArguments where

import           Control.Monad               (foldM)
import           Data.TypeDesc
import           Luna.IR
import           Luna.Pass.Data.ExprRoots
import           Luna.Pass.Data.UniqueNameGen
import           Luna.Prelude                 hiding (new, s, String)
import qualified Luna.Prelude                 as P
import           OCI.IR.Combinators
import           OCI.IR.Layout.Typed          hiding (Cons)
import qualified OCI.IR.Repr.Vis              as Vis
import           OCI.Pass                     (Inputs, Outputs, Pass, Preserves, SubPass)
import qualified OCI.Pass                     as Pass
import           System.Log


data BlankDesugaring
type instance Abstract   BlankDesugaring = BlankDesugaring
type instance Inputs     Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Inputs     Attr  BlankDesugaring = '[UniqueNameGen, ExprRoots]
type instance Inputs     Event BlankDesugaring = '[]

type instance Outputs    Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // Type]
type instance Outputs    Attr  BlankDesugaring = '[UniqueNameGen, ExprRoots]
type instance Outputs    Event BlankDesugaring = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Preserves        BlankDesugaring = '[]

runBlankDesugaring :: (MonadIR m, MonadPassManager m) => Pass BlankDesugaring m
runBlankDesugaring = do
    roots    <- getAttr @ExprRoots
    newRoots <- mapM desugar $ unwrap roots
    putAttr @ExprRoots $ wrap newRoots


desugar :: (MonadIR m, MonadPassManager m) => Expr Draft -> SubPass BlankDesugaring m (Expr Draft)
desugar root = do
    res <- replaceBlanks root
    replaceWithLam root res

runReplaceBlanks :: forall m. (MonadIR m, MonadPassManager m)
                 => Expr Draft -> SubPass BlankDesugaring m [Expr Var]
runReplaceBlanks e = do
    res <- replaceBlanks e
    replaceWithLam e res
    return []

replaceWithLam :: forall m. (MonadIR m, MonadPassManager m)
          => Expr Draft -> [Expr Var] -> SubPass BlankDesugaring m (Expr Draft)
replaceWithLam e vars = if null vars then return e else do
    tmpBlank <- blank
    newNode  <- lams (generalize <$> vars) $ generalize tmpBlank
    substitute newNode e
    replace e tmpBlank
    return newNode

replaceBlanks :: forall m. (MonadIR m, MonadPassManager m)
              => Expr Draft -> SubPass BlankDesugaring m [Expr Var]
replaceBlanks e = matchExpr e $ \case
    Blank         -> do
        v <- var =<< genName
        replace v e
        return [v]
    Grouped g         -> runReplaceBlanks =<< source g
    Marked _ b        -> runReplaceBlanks =<< source b
    Lam _ o           -> runReplaceBlanks =<< source o
    ASGFunction _ _ b -> runReplaceBlanks =<< source b
    Unify _ r         -> runReplaceBlanks =<< source r
    Seq l r           -> do
        runReplaceBlanks =<< source l
        runReplaceBlanks =<< source r
    App f a           -> (++) <$> (replaceBlanks =<< source f) <*> (replaceBlanks =<< source a)
    Acc v _           -> replaceBlanks =<< source v
    AccSection ns     -> do
        x    <- var =<< genName
        accs <- foldM (fmap generalize .: acc) (generalize x :: Expr Draft) ns
        l    <- lam x accs
        replace l e
        return []
    LeftSection o b -> do
        op   <- source o
        body <- source b
        runReplaceBlanks op
        runReplaceBlanks body
        x  <- var =<< genName
        ap  <- app op x
        ap2 <- app ap body
        l   <- lam x ap2
        replace l e
        return []
    RightSection o b -> do
        op   <- source o
        body <- source b
        runReplaceBlanks op
        runReplaceBlanks body
        ap <- app op body
        replace ap e
        return []
    Match t cls      -> do
        runReplaceBlanks =<< source t
        mapM (runReplaceBlanks <=< source) cls
        return []
    _             -> return []

lams :: (MonadRef m, Emitter (New // AnyExprLink) m, Emitter (New // AnyExpr) m,
         Writer Net AnyExprLink m, Writer Net AnyExpr m) => [Expr Draft] -> Expr Draft -> m (Expr Draft)
lams args output = unsafeRelayout <$> foldM (flip lamAny) (unsafeRelayout output) (unsafeRelayout <$> reverse args)

lamAny :: (Writer Net AnyExprLink m, Writer Net AnyExpr m, MonadRef m,
           Emitter (New // AnyExprLink) m, Emitter (New // AnyExpr) m) => Expr Draft -> Expr Draft -> m (Expr Draft)
lamAny a b = fmap generalize $ lam a b
