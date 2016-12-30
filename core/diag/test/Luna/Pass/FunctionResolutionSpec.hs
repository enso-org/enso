{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Pass.FunctionResolutionSpec where

import qualified Control.Monad.State.Dependent.Old as S
import Luna.Prelude hiding (String)
import qualified Luna.Prelude as P
import Test.Hspec   (Spec, describe, it, shouldBe)
import Luna.TestUtils
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import Data.TypeVal
import System.Log

-- impl imports, to refactor

import Luna.IR.Function.Definition as Function
import Luna.IR.Function
import Luna.IR.Module.Definition   as Module
import Luna.IR.Runner
import Luna.IR
import Luna.IR.Expr.Combinators
import Luna.IR.Name                (Name)
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)


-- impl

newtype Imports = Imports (Map Name Module)
makeWrapped ''Imports
type instance PassAttr Imports t = Imports

data CurrentVar
type instance PassAttr CurrentVar FunctionResolution = Expr (ENT Var (E String) Draft)


data    ImportError = SymbolNotFound | SymbolAmbiguous [Name] deriving (Show, Eq)

data FunctionResolution
type instance Abstract  FunctionResolution = FunctionResolution
type instance Inputs    FunctionResolution = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, Type, Succs, UID] <> ExprLinkLayers '[Model, UID]  <> '[Attr CurrentVar, Attr Imports]
type instance Outputs   FunctionResolution = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID]              <> ExprLinkLayers '[Model, UID]
type instance Events    FunctionResolution = '[NEW // EXPR, NEW // LINK' EXPR]
type instance Preserves FunctionResolution = '[]


lookupSym :: Name -> Imports -> Either ImportError CompiledFunction
lookupSym n imps = let modulesWithMatchInfo = (over _2 $ flip Module.lookupFunction n) <$> Map.assocs (unwrap imps)
                       matchedModules       = filter (isJust . snd) modulesWithMatchInfo
                   in case matchedModules of
                      []            -> Left  SymbolNotFound
                      [(_, Just f)] -> Right f
                      matches       -> Left . SymbolAmbiguous . fmap fst $ matches

importVar :: (IRMonad m, MonadIO m, MonadPassManager m) => SubPass FunctionResolution m (Either ImportError AnyExpr)
importVar = do
    var      <- readAttr @CurrentVar
    print var
    nameLink <- view name <$> match' var
    nameNode <- source nameLink
    name     <- view lit  <$> match' nameNode
    imports  <- readAttr @Imports
    let fun  =  lookupSym (fromString name) imports
    case fun of
        Left  err  -> return $ Left err
        Right sym  -> do
            newExpr <- importFunction sym
            return $ Right newExpr

-- spec

snapshotVis :: (IRMonad m, MonadIO m, MonadPassManager m, Vis.MonadVis m) => P.String -> Pass TestPass m
snapshotVis = Vis.snapshot

testImports :: IO Imports
testImports = do
    Right id'    <- runGraph $ do
        bl <- blank
        l  <- lam (arg bl) bl
        compile $ generalize l
    Right const' <- runGraph $ do
        bl1 <- blank
        bl2 <- blank
        li  <- lam (arg bl2) bl1
        l   <- lam (arg bl1) li
        compile $ generalize l
    let mod = Module Map.empty $ Map.fromList [("id", id'), ("const", const')]
    return $ Imports $ Map.singleton "Stdlib" mod

initialize :: (IRMonad m, MonadIO m, MonadPassManager m, Vis.MonadVis m) => SubPass TestPass m (Expr (ENT Var (E String) Draft))
initialize = do
    n   <- string "id"
    unsafeRelayout <$> var n

runTest = do
    imps <- testImports
    withVis $ dropLogs $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right v <- Pass.eval' initialize
        Pass.eval' $ snapshotVis "s1"
        setAttr (typeVal' @Imports) imps
        setAttr (typeVal' @CurrentVar) v
        print v
        Pass.eval' importVar
        Pass.eval' $ snapshotVis "s2"


spec :: Spec
spec = describe "nothing" $ it "nothings" $ do
    runTest
    1 `shouldBe` 1
