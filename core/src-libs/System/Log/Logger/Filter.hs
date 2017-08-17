{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Filter where

import Prologue_old

import System.Log.Logger.Class
import System.Log.Data
import Type.List (In)

import qualified Data.Set as Set
import           Data.Set (Set)



--------------------------
-- === FilterLogger === --
--------------------------

-- === Definition === --

data FILTER
type FilterLogger = StateLogger FILTER
type instance StateOf FILTER m = m Bool

type MonadFilterLogger = MonadLoggerState FILTER


-- === Utils === --

checkLog :: MonadFilterLogger m => m Bool
checkLog = join $ getLoggerState @FILTER ; {-# INLINE checkLog #-}

runFilterLogger :: Monad m => Logger FilterLogger m Bool -> Logger FilterLogger m a -> m a
runFilterLogger = flip evalStateLogger ; {-# INLINE runFilterLogger #-}


-- === Instances === --

instance MonadLogging m => MonadLogging (Logger FilterLogger m) where
    runLoggers = whenM (not <$> checkLog) $ lift runLoggers ; {-# INLINE runLoggers #-}



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
    preTagged  = lift $ filterPreTagged  @ok @tag ; {-# INLINE preTagged  #-}
    postTagged = lift $ filterPostTagged @ok @tag ; {-# INLINE postTagged #-}
    inTagged   = lift . filterInTagged   @ok @tag ; {-# INLINE inTagged   #-}

-- Helpers

class Monad m => MonadFilterTagged (ok :: Bool) tag m where
    filterPreTagged  :: m ()
    filterPostTagged :: m ()
    filterInTagged   :: forall n. Monad n => n () -> m (n ())

instance Monad m => MonadFilterTagged 'False tag m where
    filterPreTagged  = return ()          ; {-# INLINE filterPreTagged  #-}
    filterPostTagged = return ()          ; {-# INLINE filterPostTagged #-}
    filterInTagged _ = return $ return () ; {-# INLINE filterInTagged   #-}

instance MonadTagged tag m => MonadFilterTagged 'True tag m where
    filterPreTagged  = preTagged  @tag ; {-# INLINE filterPreTagged  #-}
    filterPostTagged = postTagged @tag ; {-# INLINE filterPostTagged #-}
    filterInTagged   = inTagged   @tag ; {-# INLINE filterInTagged   #-}



------------------------------------
-- === DynamicTagFilterLogger === --
------------------------------------

-- === Definition === --

data DYNAMICTAGFILTER
type DynamicTagFilterLogger = StateLogger DYNAMICTAGFILTER
type instance StateOf DYNAMICTAGFILTER m = Set DynTag


-- === Utils === --

runDynamicTagFilterLogger :: forall m a. Monad m
                          => [DynTag] -> Logger DynamicTagFilterLogger m a -> m a
runDynamicTagFilterLogger s l = evalStateLogger l $ Set.fromList s ; {-# INLINE runDynamicTagFilterLogger #-}

runDynamicTagFilterLogger' :: forall (tags :: [*]) m a. (Monad m, Typeables tags)
                           => Logger DynamicTagFilterLogger m a -> m a
runDynamicTagFilterLogger' = runDynamicTagFilterLogger $ typeReps' @tags ; {-# INLINE runDynamicTagFilterLogger' #-}


-- === Instances === --

instance (MonadTagged tag m, Typeable tag)
      => MonadTagged tag (Logger DynamicTagFilterLogger m) where
    preTagged  = whenM (Set.member (typeRep' @tag) <$> getLoggerState @DYNAMICTAGFILTER) $ lift $ preTagged  @tag ; {-# INLINE preTagged  #-}
    postTagged = whenM (Set.member (typeRep' @tag) <$> getLoggerState @DYNAMICTAGFILTER) $ lift $ postTagged @tag ; {-# INLINE postTagged #-}
    inTagged f = do
        s <- getLoggerState @DYNAMICTAGFILTER
        if Set.member (typeRep' @tag) s then lift $ inTagged @tag f
                                        else return $ return ()
    {-# INLINE inTagged #-}
