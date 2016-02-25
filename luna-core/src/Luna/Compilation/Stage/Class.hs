module Luna.Compilation.Stage.Class where

import Prelude.Luna
import Control.Monad.Identity


-- === Definitions === --

type        StageMonad  stage = StageMonadT stage Identity
type family StageMonadT stage (m :: * -> *) :: * -> *

type             MonadStage  stage = MonadStageT stage Identity
class Monad m => MonadStageT stage m where
    runT :: stage -> StageMonadT stage m a -> m a


-- === Utils === --

run :: MonadStage stage => stage -> StageMonad stage a -> a
run = runIdentity ∘∘ runT
