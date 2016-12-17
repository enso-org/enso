{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Filter where

import Prologue
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import System.Log.Logger.Class
import System.Log.Data
import Type.List (In)



--------------------------
-- === FilterLogger === --
--------------------------

-- === Definition === --

data FILTER
type FilterLogger = StateLogger FILTER
type instance StateOf FILTER m = m Bool


-- === Utils === --

type MonadFilterLogger = MonadLoggerState FILTER

checkLog :: MonadFilterLogger m => m Bool
checkLog = join $ getLoggerState @FILTER ; {-# INLINE checkLog #-}

instance MonadLogging m => MonadLogging (Logger FilterLogger m) where
    runLoggers = whenM (not <$> checkLog) $ lift runLoggers ; {-# INLINE runLoggers #-}

-- runFilterLogger :: Monad m => Filterter (Logger FilterLogger m) Doc -> Logger FilterLogger m a -> m a
-- runFilterLogger = flip evalStateLogger ; {-# INLINE runFilterLogger #-}
--




-----------------------------------
-- === StaticTagFilterLogger === --
-----------------------------------

-- === Definition === --

data STATICTAGFILTER (tags :: [*])
type StaticTagFilterLogger tags = IdentityLogger (STATICTAGFILTER tags)


-- === Utils === --

runStaticTagFilterLogger :: forall tags m a. Logger (StaticTagFilterLogger tags) m a -> m a
runStaticTagFilterLogger = runIdentityLogger ; {-# INLINE runStaticTagFilterLogger #-}


-- === Instances === --

instance (ok ~ (tag `In` tags), MonadFilterTagged ok tag m, MonadTagged tag m)
      => MonadTagged tag (Logger (StaticTagFilterLogger tags) m) where
    preTagged  = lift $ preTagged  @tag       ; {-# INLINE preTagged  #-}
    postTagged = lift $ postTagged @tag       ; {-# INLINE postTagged #-}
    inTagged   = lift . filterTagged @ok @tag ; {-# INLINE inTagged   #-}

-- Helpers

class Monad m => MonadFilterTagged (ok :: Bool) tag m where
    filterTagged :: forall n. Monad n => n () -> m (n ())

instance Monad m => MonadFilterTagged 'False tag m where
    filterTagged _ = return $ return () ; {-# INLINE filterTagged #-}

instance MonadTagged tag m => MonadFilterTagged 'True tag m where
    filterTagged = inTagged @tag ; {-# INLINE filterTagged #-}
