module System.Log.Logger.Writer where

import Prologue
import System.Log.Logger.Class
import System.Log.Data


--------------------------
-- === WriterLogger === --
--------------------------

-- === Definition === --

data WRITER t
type WriterLogger t = StateLogger (WRITER t)
type instance StateOf (WRITER t) _ = [LogData t]


-- === Utils === --

execWriterLogger :: forall t m a. Monad m => Logger (WriterLogger t) m a -> m [DataOf t]
execWriterLogger = (fmap.fmap) unwrap' . flip execStateLogger mempty ; {-# INLINE execWriterLogger #-}


-- === Instances === --

instance DataStore t m => IsLogger (WriterLogger t) m where
    runLogger = modifyLoggerState_ @(WRITER t) . (:) =<< getData @t ; {-# INLINE runLogger #-}
