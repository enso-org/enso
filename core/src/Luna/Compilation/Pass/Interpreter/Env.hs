module Luna.Compilation.Pass.Interpreter.Env where

import           Prologue

import           Luna.Compilation.Pass.Interpreter.Class


data Env node = Env { _nodesToEval :: [node]
                    } deriving Show

makeLenses ''Env


addNodeToEval :: InterpreterMonad (Env node) m => node -> m ()
addNodeToEval node = modify_ (over nodesToEval (node :))

clearNodesToEval :: InterpreterMonad (Env node) m => m ()
clearNodesToEval = modify_ (set nodesToEval def)

getNodesToEval :: InterpreterMonad (Env node) m => m [node]
getNodesToEval = view nodesToEval <$> get


instance Default (Env node) where
    def = Env def
