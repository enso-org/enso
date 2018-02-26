module OCI.Pass.Manager where

import Prologue

import Control.Monad.State.Layered (MonadState, StateT)
import Data.Map.Strict             (Map)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map



---------------------------
-- === ComponentInfo === --
---------------------------

-- === Definition === --

data ComponentInfo = ComponentInfo
    { _byteSize :: !Int
    } deriving (Show)



-------------------------
-- === PassManager === --
-------------------------

-- === Definition === --

data PassManagerState = PassManagerState
    { _components :: Map SomeTypeRep ComponentInfo
    } deriving (Show)

type MonadPassManager m = MonadState PassManagerState m
type PassManagerT = StateT PassManagerState


-- === Running === --

evalPassManagerT :: MonadPassManager m => PassManagerT m a -> m a
evalPassManagerT = State.evalDefT


-- === Component management === --

-- registerComponent :: 


-- === Instances === --

instance Default PassManagerState where
    def = PassManagerState def ; {-# INLINE def #-}
