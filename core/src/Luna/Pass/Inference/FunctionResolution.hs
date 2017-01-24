module Luna.Pass.Inference.FunctionResolution where

import Luna.Prelude hiding (String)
import Luna.IR.Function.Definition as Function
import Luna.IR.Function
import Luna.IR.Imports
import Luna.IR.Module.Definition   as Module
import Luna.IR
import Luna.IR.Expr.Combinators
import Luna.IR.Layer.Redirect
import Luna.IR.Name                (Name)
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)
import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass



-- === Definitions === --

newtype CurrentVar = CurrentVar (Expr Var)
makeWrapped ''CurrentVar


-- === Errors === --

data ImportError = SymbolNotFound
                 | SymbolAmbiguous [Name]
                 deriving (Show, Eq)


-- === Pass === --

data FunctionResolution
type instance Abstract  FunctionResolution = FunctionResolution

type instance Inputs     Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID, AnyExpr // Redirect]
type instance Inputs     Attr  FunctionResolution = '[CurrentVar, Imports]
type instance Inputs     Event FunctionResolution = '[]

type instance Outputs    Net   FunctionResolution = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer FunctionResolution = '[AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // UID, AnyExprLink // Model, AnyExprLink // UID]
type instance Outputs    Attr  FunctionResolution = '[]
type instance Outputs    Event FunctionResolution = '[New // AnyExpr, New // AnyExprLink, Import // AnyExpr, Import // AnyExprLink]

type instance Preserves        FunctionResolution = '[]


lookupSym :: Name -> Imports -> Either ImportError CompiledFunction
lookupSym n imps = case matchedModules of
    []            -> Left  SymbolNotFound
    [(_, Just f)] -> Right f
    matches       -> Left . SymbolAmbiguous $ fst <$> matches
    where modulesWithMatchInfo = (over _2 $ flip Module.lookupFunction n) <$> Map.assocs (unwrap imps)
          matchedModules       = filter (isJust . snd) modulesWithMatchInfo

importVar :: (MonadRef m, MonadPassManager m) => SubPass FunctionResolution m (Either ImportError SomeExpr)
importVar = do
    var  <- unwrap' <$> readAttr @CurrentVar
    name <- fmap (view lit) . match' =<< source =<< view name <$> match' var
    fun  <- lookupSym (fromString name) <$> readAttr @Imports
    mapM importFunction fun
