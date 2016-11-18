module Luna.Pass.Evaluation.Interpreter.Env where

import           Prologue hiding (s)

import           Luna.Pass.Evaluation.Interpreter.Class
import           Luna.Pass.Evaluation.Interpreter.Value (Value)

import           Data.Map (Map)
import qualified Data.Map as Map

newtype Scope = Scope (Map String Value)
makeWrapped ''Scope

mergeScopes :: Scope -> Scope -> Scope
mergeScopes (Scope a) (Scope b) = Scope $ Map.union a b

lookupScope :: String -> Scope -> Maybe Value
lookupScope v (Scope a) = Map.lookup v a

data Env node = Env { _nodesToEval :: [node]
                    , _varScope    :: Scope
                    }

makeLenses ''Env


addNodeToEval :: InterpreterMonad (Env node) m => node -> m ()
addNodeToEval node = modify_ (over nodesToEval (node :))

clearNodesToEval :: InterpreterMonad (Env node) m => m ()
clearNodesToEval = modify_ (set nodesToEval def)

getNodesToEval :: InterpreterMonad (Env node) m => m [node]
getNodesToEval = view nodesToEval <$> get

enrichScope :: InterpreterMonad (Env node) m => Scope -> m ()
enrichScope s = modify_ $ over varScope $ mergeScopes s

lookupVar :: InterpreterMonad (Env node) m => String -> m (Maybe Value)
lookupVar v = lookupScope v . view varScope <$> get

instance Default (Env node) where
    def = Env def def

instance Default Scope where
    def = Scope def
