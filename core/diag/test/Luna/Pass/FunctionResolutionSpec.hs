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
import Data.TypeDesc
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

newtype CurrentVar = CurrentVar (Expr (ENT Var (E String) Draft))

data    ImportError = SymbolNotFound | SymbolAmbiguous [Name] deriving (Show, Eq)

data FunctionResolution
type instance Abstract  FunctionResolution = FunctionResolution

type instance Inputs     Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID]
type instance Inputs     Attr  FunctionResolution = '[CurrentVar, Imports]
type instance Inputs     Event FunctionResolution = '[]

type instance Outputs    Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID]
type instance Outputs    Attr  FunctionResolution = '[]
type instance Outputs    Event FunctionResolution = '[New // AnyExpr, New // AnyExprLink, Import // AnyExpr, Import // AnyExprLink]

type instance Preserves        FunctionResolution = '[]


lookupSym :: Name -> Imports -> Either ImportError CompiledFunction
lookupSym n imps = let modulesWithMatchInfo = (over _2 $ flip Module.lookupFunction n) <$> Map.assocs (unwrap imps)
                       matchedModules       = filter (isJust . snd) modulesWithMatchInfo
                   in case matchedModules of
                      []            -> Left  SymbolNotFound
                      [(_, Just f)] -> Right f
                      matches       -> Left . SymbolAmbiguous . fmap fst $ matches

importVar :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass FunctionResolution m (Either ImportError SomeExpr)
importVar = do
    (CurrentVar var) <- readAttr @CurrentVar
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

snapshotVis :: (MonadRef m, MonadIO m, MonadPassManager m, Vis.MonadVis m) => P.String -> Pass TestPass m
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

initialize :: (MonadRef m, MonadIO m, MonadPassManager m, Vis.MonadVis m) => SubPass TestPass m (Expr (ENT Var (E String) Draft))
initialize = do
    n   <- string "id"
    unsafeRelayout <$> var n

runTest = do
    imps <- testImports
    withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right v <- Pass.eval' initialize
        Pass.eval' $ snapshotVis "s1"
        setAttr (getTypeDesc @Imports) imps
        setAttr (getTypeDesc @CurrentVar) v
        print v
        Pass.eval' importVar
        Pass.eval' $ snapshotVis "s2"


spec :: Spec
spec = describe "nothing" $ it "nothings" $ do
    runTest
    1 `shouldBe` 1
