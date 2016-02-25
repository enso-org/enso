module Luna.Compilation.Pass.Dirty.Data.Env where

import           Prologue

import           Luna.Compilation.Pass.Dirty.Monad



data Env node = Env { _reqNodes :: [node]
                    } deriving Show

makeLenses ''Env


addReqNode :: DirtyMonad (Env node) m => node -> m ()
addReqNode node = modify_ (over reqNodes (node :))

clearReqNodes :: DirtyMonad (Env node) m => m ()
clearReqNodes = modify_ (set reqNodes def)

getReqNodes :: DirtyMonad (Env node) m => m [node]
getReqNodes = view reqNodes <$> get


instance Default (Env node) where
    def = Env def
