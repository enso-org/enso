module Luna.Builtin.Data.Module where

import Prologue_old
import Luna.Builtin.Data.Function
import Luna.Builtin.Data.LunaValue (LunaValue)
import Luna.Builtin.Data.Class
import OCI.IR.Name
import OCI.IR
import Luna.IR.Layer.Errors (CompileError)

import           Data.Map                    (Map)
import qualified Data.Map                    as Map


data CompiledWorld = CompiledWorld { _classes   :: Map SomeExpr Class
                                   , _functions :: Map SomeExpr (Either [CompileError] Function)
                                   }
makeLenses ''CompiledWorld

unionWorlds :: CompiledWorld -> CompiledWorld -> CompiledWorld
unionWorlds (CompiledWorld c1 f1) (CompiledWorld c2 f2) = CompiledWorld (Map.union c1 c2) (Map.union f1 f2)

unionsWorlds :: [CompiledWorld] -> CompiledWorld
unionsWorlds = foldr unionWorlds def

instance Default CompiledWorld where
    def = CompiledWorld def def

data Imports = Imports { _importedClasses   :: Map Name (WithDocumentation Class)
                       , _importedFunctions :: Map Name (WithDocumentation (Either [CompileError] Function))
                       }
makeLenses ''Imports

unionImports :: Imports -> Imports -> Imports
unionImports (Imports c1 f1) (Imports c2 f2) = Imports (Map.union c1 c2) (Map.union f1 f2)

unionsImports :: [Imports] -> Imports
unionsImports = foldr unionImports def

instance Default Imports where
    def = Imports def def

getObjectMethodMap :: Name -> Imports -> Map Name (Either [CompileError] LunaValue)
getObjectMethodMap className imps = fmap (view value) . view documentedItem <$> view (documentedItem . methods) klass where
    klass = case Map.lookup className (imps ^. importedClasses) of
        Nothing -> error $ "ObjectMethodMap for: " ++ show className ++ " does not exist."
        Just a  -> a

getConstructorMethodMap :: Name -> Imports -> Map Name (Either [CompileError] LunaValue)
getConstructorMethodMap consName imps = fmap (view value) . view documentedItem <$> view methods klass where
    klass = case catMaybes $ fmap (whenHasConstructor consName) $ imps ^.. importedClasses . traverse . documentedItem of
        [] -> error $ "Class with a constructor: " ++ show consName ++ " does not exist."
        a : _ -> a
