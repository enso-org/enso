{-# LANGUAGE PartialTypeSignatures #-}

module Luna.Pass.Desugaring.BlankArguments where

import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass        as Pass

import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.TypeDesc
import qualified Luna.IR.Repr.Vis as Vis
import Luna.IR.Expr.Combinators
import Luna.IR.Function hiding (args)
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import Luna.IR
import System.Log
import Control.Monad (foldM)



data BlankDesugaring
type instance Abstract   BlankDesugaring = BlankDesugaring
type instance Inputs     Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Inputs     Attr  BlankDesugaring = '[UniqueNameGen, UsedVars]
type instance Inputs     Event BlankDesugaring = '[]

type instance Outputs    Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // Type]
type instance Outputs    Attr  BlankDesugaring = '[UniqueNameGen, UsedVars]
type instance Outputs    Event BlankDesugaring = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]

type instance Preserves        BlankDesugaring = '[]


desugar :: forall m. (MonadIR m, MonadPassManager m, _)
        => SomeExpr -> SubPass BlankDesugaring m SomeExpr
desugar e = do
    e'            <- replaceBlanks e
    UsedVars vars <- readAttr @UsedVars
    newExpr       <- lams (map unsafeRelayout $ reverse vars) e'
    replaceNode e newExpr
    deleteSubtree e
    return newExpr


newtype UniqueNameGen = UniqueNameGen (P.String, Int)

newtype UsedVars = UsedVars [Expr $ Var #> String]

genName :: (MonadIR m) => SubPass BlankDesugaring m P.String
genName = do
    UniqueNameGen (base, number) <- readAttr
    writeAttr $ UniqueNameGen (base, number + 1)
    return $ '^' : base ++ show number

obscureName :: P.String
obscureName = "^obscureName0"

localAttr :: forall attr pass m a. _ => _ -> SubPass pass m a -> SubPass pass m a
localAttr newAttr act = do
    st <- readAttr @attr
    writeAttr @attr newAttr
    res <- act
    writeAttr @attr st
    return res

modifyAttr :: forall attr pass m. _ => (_ -> _) -> SubPass pass m ()
modifyAttr f = do
    st <- readAttr @attr
    writeAttr @attr $ f st

replaceBlanks :: forall m. (MonadIR m, MonadPassManager m)
              => SomeExpr -> SubPass BlankDesugaring m SomeExpr
replaceBlanks e = match e $ \case
    -- interesting cases:

    -- blank is replaced by new name var and this var is saved
    -- for reuse in lambda
    Blank -> do
        n <- genName
        v <- strVar n
        replaceNode e v
        modifyAttr $ \(UsedVars s) -> UsedVars (v:s)
        deleteSubtree e
        return $ unsafeRelayout v
    -- grouped starts new desugaring environment
    Grouped g -> do
        g' <- source g
        desu <- localAttr (UsedVars []) $ desugar g'
        unsafeRelayout <$> grouped desu
    Lam (Arg _ v) f -> do
        v' <- source v
        f' <- source f >>= localAttr (UsedVars []) . desugar
        unsafeRelayout <$> lam (arg v') f'

    -- these just cut through constructors
    App f (Arg _ a) -> do
        f' <- source f >>= replaceBlanks
        a' <- source a >>= replaceBlanks
        unsafeRelayout <$> app f' (arg a')
    Acc n v -> do
        n' <- source n
        v' <- source v >>= replaceBlanks
        unsafeRelayout <$> acc n' v'
    Integer{} -> return e
    Rational{} -> return e
    String{} -> return e
    Cons{} -> return e
    Var{} -> return e
    Star -> return e
    Missing -> return e
    Unify{} -> return e

lams :: _ => [SomeExpr] -> SomeExpr -> m SomeExpr
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout output) (unsafeRelayout <$> reverse args)
    where
        f arg' lam' = lamAny (arg lam') arg'

lamAny :: _ => Arg SomeExpr -> SomeExpr -> m SomeExpr
lamAny a b = fmap generalize $ lam a b
