module Luna.Pass.Inference.FunctionResolution where

import Luna.Prelude hiding (String)
import Luna.IR.Function.Definition as Function
import Luna.IR.Function
import Luna.IR.Module.Definition   as Module
import Luna.IR
import Luna.IR.Expr.Combinators
import Luna.IR.Name                (Name)
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Data.Maybe        (isJust)
import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass

newtype Imports = Imports (Map Name Module)
makeWrapped ''Imports

newtype CurrentVar  = CurrentVar (Expr (ENT Var (E String) Draft))
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

