module Luna.Core.ST where

import Prelude                      hiding ((.))
import Control.Monad.Primitive
import Data.Functor.Utils
import GHC.STRef                    (STRef, readSTRef, writeSTRef)


type STRef' m = STRef (PrimState m)


-- === Utils === --

modifySTRefM :: PrimMonad m => STRef' m t -> (t -> m (t, a)) -> m a
modifySTRefM ref f = do
    (!t, a) <- f =<< liftPrim (readSTRef ref)
    a <$ liftPrim (writeSTRef ref t)
{-# INLINE modifySTRefM #-}

modifySTRefM_ :: PrimMonad m => STRef' m t -> (t -> m t) -> m ()
modifySTRefM_ ref f = modifySTRefM ref (fmap2 (,()) f) ; {-# INLINE modifySTRefM_ #-}

modifySTRef_ :: PrimMonad m => STRef' m t -> (t -> t) -> m ()
modifySTRef_ ref f = modifySTRefM_ ref (return . f) ; {-# INLINE modifySTRef_ #-}

