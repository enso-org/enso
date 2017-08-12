module Prologue.Data.Maybe (module Prologue.Data.Maybe, module X) where

import Prelude hiding (mempty, fail)

import Data.Maybe                 as X (Maybe(Just, Nothing), maybe, isJust, isNothing, fromMaybe, catMaybes, mapMaybe)
import Control.Monad.Trans.Maybe  as X (MaybeT(MaybeT), runMaybeT, mapMaybeT, maybeToExceptT, exceptToMaybeT)
import Control.Error.Util         as X (maybeT, just, nothing, isJustT, isNothingT)

import Control.Applicative
import Control.Monad       hiding (fail)
import Control.Monad.Fail
import Data.Convert
import Data.Monoids
import Prologue.Data.Basic

import qualified Data.Maybe as M


{-# WARNING unsafeFromJust "Do not use in production code" #-}
unsafeFromJust :: Maybe a -> a
unsafeFromJust = M.fromJust ; {-# INLINE unsafeFromJust #-}

unsafeFromJustM :: (Monad m, MonadFail m) => Maybe a -> m a
unsafeFromJustM = \case
    Just a  -> pure a
    Nothing -> fail "Prelude.fromJustM: Nothing"
{-# INLINE unsafeFromJustM #-}

fromMaybeWith :: (a -> b) -> b -> Maybe a -> b
fromMaybeWith f b = \case
    Just  a -> f a
    Nothing -> b
{-# INLINE fromMaybeWith #-}

fromMaybeWithM :: (a -> m b) -> m b -> Maybe a -> m b
fromMaybeWithM f mb = \case
    Just a  -> f a
    Nothing -> mb
{-# INLINE fromMaybeWithM #-}

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM = fromMaybeWithM pure ; {-# INLINE fromMaybeM #-}

justIf :: ToBool' cond => cond -> a -> Maybe a
justIf cond a = iff cond (Just a) Nothing ; {-# INLINE justIf #-}

withJust   :: (Applicative m, Mempty out) =>    Maybe a  -> (a -> m out) -> m out
withJust_  :: Applicative m               =>    Maybe a  -> (a -> m out) -> m ()
withJustM  :: (Monad m, Mempty out)       => m (Maybe a) -> (a -> m out) -> m out
withJustM_ :: Monad m                     => m (Maybe a) -> (a -> m out) -> m ()
withJust   ma f = maybe (pure mempty) f          ma ; {-# INLINE withJust   #-}
withJust_  ma f = maybe (pure ())     (void . f) ma ; {-# INLINE withJust_  #-}
withJustM  ma f = flip withJust  f =<< ma           ; {-# INLINE withJustM  #-}
withJustM_ ma f = flip withJust_ f =<< ma           ; {-# INLINE withJustM_ #-}
