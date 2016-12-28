{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Pass.FunctionResolutionSpec where

import Luna.Prelude hiding (String)
import Test.Hspec   (Spec, describe, it)
import Luna.TestUtils
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass

-- impl imports, to refactor

import Luna.IR.Function.Definition as Function
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

data    ImportError = SymbolNotFound | SymbolAmbiguous [Name] deriving (Show, Eq)

data FunctionResolution
type instance Abstract  FunctionResolution = FunctionResolution
type instance Inputs    FunctionResolution = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model] <> '[Attr WorkingElem, Attr Imports]
type instance Outputs   FunctionResolution = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Events    FunctionResolution = '[NEW // EXPR, NEW // LINK' EXPR]
type instance Preserves FunctionResolution = '[]

lookupSym :: Name -> Imports -> Either ImportError CompiledFunction
lookupSym n imps = let modulesWithMatchInfo = (over _2 $ flip Module.lookupFunction n) <$> Map.assocs (unwrap imps)
                       matchedModules       = filter (isJust . snd) modulesWithMatchInfo
                   in case matchedModules of
                      []            -> Left  SymbolNotFound
                      [(_, Just f)] -> Right f
                      matches       -> Left . SymbolAmbiguous . fmap fst $ matches

importVar :: (IRMonad m, MonadIO m, MonadPassManager m) => Expr (ENT Var (E String) Draft) -> SubPass FunctionResolution m (Either ImportError AnyExpr)
importVar v = do
    nameLink <- view name <$> match' v
    nameNode <- source nameLink
    name     <- view lit  <$> match' nameNode
    return $ Left SymbolNotFound

-- spec



spec :: Spec
spec = return ()
