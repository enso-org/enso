module Prologue.Data.Maybe (module Prologue.Data.Maybe, module X) where

import Prelude hiding ((.), mempty, fail)

import Data.Maybe                 as X (Maybe(Just, Nothing), maybe, isJust, isNothing, catMaybes, mapMaybe)
import Control.Monad.Trans.Maybe  as X (MaybeT(MaybeT), runMaybeT, mapMaybeT, maybeToExceptT, exceptToMaybeT)
import Control.Error.Util         as X (maybeT, just, nothing, isJustT, isNothingT)

import Control.Monad       hiding (fail)
import Control.Monad.Fail
import Data.Monoids
import Data.Functor.Utils
import Prologue.Data.Basic

import qualified Data.Maybe as M


-- === Conditionals === --

justIf   ::              Bool -> a -> Maybe a
justWhen :: (Monad m) => Bool -> a -> m (Maybe a)
justIf p = ifThenMempty p . Just ; {-# INLINE justIf   #-}
justWhen = return .: justIf      ; {-# INLINE justWhen #-}


-- === FromJust === --

{-# WARNING fromMaybe "use fromJust instead" #-}
fromMaybe ::                    a -> Maybe a ->   a
fromJust  ::                    a -> Maybe a ->   a
fromJustM :: Applicative m => m a -> Maybe a -> m a
fromMaybe   = fromJust     ; {-# INLINE fromMaybe #-}
fromJust  d = maybe d id   ; {-# INLINE fromJust  #-}
fromJustM d = maybe d pure ; {-# INLINE fromJustM #-}

{-# WARNING unsafeFromJust "Do not use in production code" #-}
unsafeFromJust  ::                           Maybe a ->   a
unsafeFromJustM :: (Monad m, MonadFail m) => Maybe a -> m a
unsafeFromJust  = M.fromJust                             ; {-# INLINE unsafeFromJust  #-}
unsafeFromJustM = maybe (fail "fromJustM: Nothing") pure ; {-# INLINE unsafeFromJustM #-}


-- === Monadic === --

withJust   :: (Applicative m, Mempty out) =>    Maybe a  -> (a -> m out) -> m out
withJust_  :: Applicative m               =>    Maybe a  -> (a -> m out) -> m ()
withJustM  :: (Monad m, Mempty out)       => m (Maybe a) -> (a -> m out) -> m out
withJustM_ :: Monad m                     => m (Maybe a) -> (a -> m out) -> m ()
withJust   t f = maybe (pure mempty) f          t ; {-# INLINE withJust   #-}
withJust_  t f = maybe (pure ())     (void . f) t ; {-# INLINE withJust_  #-}
withJustM  t f = flip withJust  f =<< t           ; {-# INLINE withJustM  #-}
withJustM_ t f = flip withJust_ f =<< t           ; {-# INLINE withJustM_ #-}

whenJust   :: (Applicative m, Mempty out) =>    Maybe a  -> m out -> m out
whenJust_  :: (Applicative m)             =>    Maybe a  -> m out -> m ()
whenJustM  :: (Monad m, Mempty out)       => m (Maybe a) -> m out -> m out
whenJustM_ :: (Monad m)                   => m (Maybe a) -> m out -> m ()
whenJust   t = withJust   t . const ; {-# INLINE whenJust   #-}
whenJust_  t = withJust_  t . const ; {-# INLINE whenJust_  #-}
whenJustM  t = withJustM  t . const ; {-# INLINE whenJustM  #-}
whenJustM_ t = withJustM_ t . const ; {-# INLINE whenJustM_ #-}

whenNothing   :: (Applicative m, Mempty out) =>    Maybe a  -> m out -> m out
whenNothing_  :: (Applicative m)             =>    Maybe a  -> m out -> m ()
whenNothingM  :: (Monad m, Mempty out)       => m (Maybe a) -> m out -> m out
whenNothingM_ :: (Monad m)                   => m (Maybe a) -> m out -> m ()
whenNothing   t f = maybe f        (const $ pure mempty) t ; {-# INLINE whenNothing   #-}
whenNothing_  t f = maybe (void f) (const $ pure ())     t ; {-# INLINE whenNothing_  #-}
whenNothingM  t f = flip whenNothing  f =<< t              ; {-# INLINE whenNothingM  #-}
whenNothingM_ t f = flip whenNothing_ f =<< t              ; {-# INLINE whenNothingM_ #-}
