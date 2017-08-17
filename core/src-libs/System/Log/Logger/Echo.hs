{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Echo where

import Prologue_old
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import System.Log.Logger.Class
import System.Log.Data


------------------------
-- === EchoLogger === --
------------------------

data ECHO
type EchoLogger = IdentityLogger ECHO

instance (MonadIO m, DataStore Msg m) => IsLogger EchoLogger m where
    runLogger = liftIO . putDoc =<< getData' @Msg ; {-# INLINE runLogger #-}

runEchoLogger :: Logger EchoLogger m a -> m a
runEchoLogger = runIdentityLogger ; {-# INLINE runEchoLogger #-}
