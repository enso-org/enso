{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Priority where

import Prologue
import qualified Type.List as List

import System.Log.Logger.Class
import System.Log.Data


----------------------------
-- === PriorityLogger === --
----------------------------

data PRIORITY (prs :: [*])
type PriorityLogger prs = IdentityLogger (PRIORITY prs)

runPriorityLogger :: forall prs m a. Logger (PriorityLogger prs) m a -> m a
runPriorityLogger = runIdentityLogger ; {-# INLINE runPriorityLogger #-}


-- === Tag priority handling === --

instance (maybeIdx ~ List.Index tag prs, HandlePriorityTag maybeIdx m)
      => MonadTag tag (Logger (PriorityLogger prs) m) where
    setTag   = handlePriorityTag @maybeIdx ; {-# INLINE setTag   #-}
    unsetTag = return ()                   ; {-# INLINE unsetTag #-}

class Monad m => HandlePriorityTag (idx :: Maybe Nat) m where
    handlePriorityTag :: forall prs. Logger (PriorityLogger prs) m ()

instance Monad m => HandlePriorityTag 'Nothing m where
    handlePriorityTag = return () ; {-# INLINE handlePriorityTag #-}

instance (DataStore Priority m, KnownNat n) => HandlePriorityTag ('Just n) m where
    handlePriorityTag = putData $ priority $ natVal (Proxy :: Proxy n) ; {-# INLINE handlePriorityTag #-}
