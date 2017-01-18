{-# LANGUAGE PartialTypeSignatures #-}

module Luna.Pass.AccessorFunctionSpec (spec) where

import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass        as Pass
import           Control.Monad.Raise (tryAll)
import qualified Luna.IR.Repr.Vis as Vis

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.Maybe (isJust)
import Data.TypeDesc
import qualified Luna.IR.Repr.Vis as Vis
import Luna.IR.Expr.Combinators
import Luna.IR.Imports
import qualified Luna.IR.Module.Definition   as Module
import qualified Data.Map          as Map
import Luna.IR.Function hiding (args)
import Luna.IR.Function.Definition
import Luna.IR.Expr.Layout
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import           Luna.IR.Name                (Name)
import Luna.IR.Class.Method        (Method(..))
import Luna.IR.Class.Definition
import Luna.IR.Runner
import Luna.IR
import Luna.TestUtils
import Luna.Pass.Inference.FunctionResolution (ImportError(..), lookupSym)
import System.Log
import Control.Monad (foldM)



newtype CurrentAcc = CurrentAcc (Expr Acc)


data AccessorFunction
type instance Abstract   AccessorFunction = AccessorFunction
type instance Inputs     Net   AccessorFunction = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer AccessorFunction = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExprLink // Type, AnyExpr // Succs]
type instance Inputs     Attr  AccessorFunction = '[CurrentAcc, Imports]
type instance Inputs     Event AccessorFunction = '[]

type instance Outputs    Net   AccessorFunction = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer AccessorFunction = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Redirect]
type instance Outputs    Attr  AccessorFunction = '[]
type instance Outputs    Event AccessorFunction = '[New // AnyExpr, New // AnyExprLink, Import // AnyExpr, Import // AnyExprLink]

type instance Preserves        AccessorFunction = '[]

data Redirect
type instance LayerData Redirect t = SomeExpr

data AccessorError = MethodNotFound P.String
                   | AmbiguousType

importAccessor :: _ => SubPass AccessorFunction m (Maybe AccessorError)
importAccessor = do
    CurrentAcc acc <- readAttr
    match acc $ \case
        Acc n v -> do
            v' <- source v
            tl <- readLayer @Type v'
            t <- source tl
            match t $ \case
                Cons cls _args -> do
                    classNameExpr  <- source cls
                    methodNameExpr <- source n
                    method         <- importMethod classNameExpr methodNameExpr
                    case method of
                        Left SymbolNotFound -> do
                            methodName <- view lit <$> match' methodNameExpr
                            return $ Just $ MethodNotFound methodName
                        Right (ImportedMethod self body) -> do
                            replaceNode (generalize self) (generalize v')
                            writeLayer @Redirect (generalize body) acc
                            unifyTypes acc body
                            unifyTypes self v'
                            return Nothing
                _ -> return $ Just AmbiguousType

unifyTypes :: _ => Expr _ -> Expr _ -> SubPass AccessorFunction m (Expr _)
unifyTypes e1 e2 = do
    t1 <- readLayer @Type e1 >>= source
    t2 <- readLayer @Type e2 >>= source
    unify t1 t2

data ImportedMethod = ImportedMethod { self :: SomeExpr, body :: SomeExpr }

importMethod :: _ => Expr _ -> Expr _ -> SubPass AccessorFunction m (Either ImportError ImportedMethod)
importMethod classExpr methodNameExpr = do
    className  <- fmap fromString (view lit <$> match' classExpr)
    methodName <- fmap fromString (view lit <$> match' methodNameExpr)
    imports    <- readAttr @Imports
    let method = (lookupClass className >=> lookupMethod methodName) imports
    case method of
        Left err -> return $ Left err
        Right (Method self body) -> do
            translator <- importTranslator body
            bodyExpr   <- importFunction body
            return $ Right $ ImportedMethod (translator self) bodyExpr

lookupClass :: Name -> Imports -> Either ImportError Class
lookupClass n imps = case matchedModules of
    []            -> Left  SymbolNotFound
    [(_, Just f)] -> Right f
    matches       -> Left . SymbolAmbiguous $ fst <$> matches
    where modulesWithMatchInfo = (over _2 $ flip Module.lookupClass n) <$> Map.assocs (unwrap imps)
          matchedModules       = filter (isJust . snd) modulesWithMatchInfo

lookupMethod :: Name -> Class -> Either ImportError Method
lookupMethod n cls = case Map.lookup n (cls ^. methods) of
    Just m -> Right m
    _      -> Left SymbolNotFound

-- test :: _ => SubPass AccessorFunction _ _
-- test = do
--     one <- integer (1::Int)
--     int <- string "Int"
--     c <- cons_ int
--     l <- link int one
--     writeLayer @Type l one
--
--     rawAcc "succ" one

spec :: Spec
spec = return ()
