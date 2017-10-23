module System.Log.Logger.Drop where

import Prologue_old
import System.Log.Logger.Class
import System.Log.Data


------------------------
-- === DropLogger === --
------------------------

data DROP
type DropLogger = IdentityLogger DROP

dropLogs :: Logger DropLogger m a -> m a
dropLogs = runIdentityLogger ; {-# INLINE dropLogs #-}


-- === Instances === --

instance Monad m => MonadLogging (Logger DropLogger m) where
    runLoggers = return () ; {-# INLINE runLoggers #-}

-- | If we use Tag logging, we just discard every action.
instance Monad m => MonadTagged tag (Logger DropLogger m) where
    preTagged  = return ()          ; {-# INLINE preTagged  #-}
    postTagged = return ()          ; {-# INLINE postTagged #-}
    inTagged _ = return $ return () ; {-# INLINE inTagged   #-}

-- This is hacky, but because Drop logger is guaranteed to drop all logs,
-- we can be sure the information will not be needed.
instance Monad m => DataStore d (Logger DropLogger m) where
    getData   = return (error "impossible") ; {-# INLINE getData #-}
    putData _ = return (error "impossible") ; {-# INLINE putData #-}
