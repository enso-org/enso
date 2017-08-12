module Prologue.Data.Either (module Prologue.Data.Either, module X) where

import Prelude (Either(Left, Right), const, id, ($), (.), flip)

import Control.Monad.Trans.Either as X (EitherT(EitherT), runEitherT, eitherT, hoistEither, left, right, swapEitherT, mapEitherT)
import Data.Either.Combinators    as X (isLeft, isRight, mapLeft, mapRight, leftToMaybe, rightToMaybe, swapEither)
import Data.Either                as X (either, partitionEithers)

import Prologue.Data.Basic
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Convert
import Data.Monoids


-- === Conditionals === --

eitherIf :: ToBool' cond => cond -> ok -> fail -> Either fail ok
eitherIf cond ok fl = iff cond (Right ok) (Left fl) ; {-# INLINE eitherIf #-}


-- === FromEither === --

fromRight  ::                    r -> Either l r ->   r
fromRightM :: Applicative m => m r -> Either l r -> m r
fromRight  d = either (const d) id   ; {-# INLINE fromRight  #-}
fromRightM d = either (const d) pure ; {-# INLINE fromRightM #-}

fromLeft  ::                    l -> Either l r ->   l
fromLeftM :: Applicative m => m l -> Either l r -> m l
fromLeft  d = either id   (const d) ; {-# INLINE fromLeft  #-}
fromLeftM d = either pure (const d) ; {-# INLINE fromLeftM #-}

{-# WARNING unsafeFromRight "Do not use in production code" #-}
unsafeFromRight  ::                           Either l r ->   r
unsafeFromRightM :: (Monad m, MonadFail m) => Either l r -> m r
unsafeFromRight (Right r) = r                                       ; {-# INLINE unsafeFromRight  #-}
unsafeFromRightM = either (const $ fail "fromRightM: Nothing") pure ; {-# INLINE unsafeFromRightM #-}

{-# WARNING unsafeFromLeft "Do not use in production code" #-}
unsafeFromLeft  ::                           Either l r ->   l
unsafeFromLeftM :: (Monad m, MonadFail m) => Either l r -> m l
unsafeFromLeft (Left r) = r                                       ; {-# INLINE unsafeFromLeft  #-}
unsafeFromLeftM = either pure (const $ fail "fromLeftM: Nothing") ; {-# INLINE unsafeFromLeftM #-}


-- === Monadic === --

withRight   :: (Applicative m, Mempty out) =>    Either l r  -> (r -> m out) -> m out
withRight_  :: Applicative m               =>    Either l r  -> (r -> m out) -> m ()
withRightM  :: (Monad m, Mempty out)       => m (Either l r) -> (r -> m out) -> m out
withRightM_ :: Monad m                     => m (Either l r) -> (r -> m out) -> m ()
withRight   ma f = either (const $ pure mempty) f          ma ; {-# INLINE withRight   #-}
withRight_  ma f = either (const $ pure ())     (void . f) ma ; {-# INLINE withRight_  #-}
withRightM  ma f = flip withRight  f =<< ma                   ; {-# INLINE withRightM  #-}
withRightM_ ma f = flip withRight_ f =<< ma                   ; {-# INLINE withRightM_ #-}

withLeft   :: (Applicative m, Mempty out) =>    Either l r  -> (l -> m out) -> m out
withLeft_  :: Applicative m               =>    Either l r  -> (l -> m out) -> m ()
withLeftM  :: (Monad m, Mempty out)       => m (Either l r) -> (l -> m out) -> m out
withLeftM_ :: Monad m                     => m (Either l r) -> (l -> m out) -> m ()
withLeft   ma f = either f          (const $ pure mempty) ma ; {-# INLINE withLeft   #-}
withLeft_  ma f = either (void . f) (const $ pure ())     ma ; {-# INLINE withLeft_  #-}
withLeftM  ma f = flip withLeft  f =<< ma                   ; {-# INLINE withLeftM  #-}
withLeftM_ ma f = flip withLeft_ f =<< ma                   ; {-# INLINE withLeftM_ #-}

whenRight   :: (Applicative m, Mempty out) =>    Either l r  -> m out -> m out
whenRight_  :: (Applicative m)             =>    Either l r  -> m out -> m ()
whenRightM  :: (Monad m, Mempty out)       => m (Either l r) -> m out -> m out
whenRightM_ :: (Monad m)                   => m (Either l r) -> m out -> m ()
whenRight   t = withRight   t . const ; {-# INLINE whenRight   #-}
whenRight_  t = withRight_  t . const ; {-# INLINE whenRight_  #-}
whenRightM  t = withRightM  t . const ; {-# INLINE whenRightM  #-}
whenRightM_ t = withRightM_ t . const ; {-# INLINE whenRightM_ #-}

whenLeft   :: (Applicative m, Mempty out) =>    Either l r  -> m out -> m out
whenLeft_  :: (Applicative m)             =>    Either l r  -> m out -> m ()
whenLeftM  :: (Monad m, Mempty out)       => m (Either l r) -> m out -> m out
whenLeftM_ :: (Monad m)                   => m (Either l r) -> m out -> m ()
whenLeft   t = withLeft   t . const ; {-# INLINE whenLeft   #-}
whenLeft_  t = withLeft_  t . const ; {-# INLINE whenLeft_  #-}
whenLeftM  t = withLeftM  t . const ; {-# INLINE whenLeftM  #-}
whenLeftM_ t = withLeftM_ t . const ; {-# INLINE whenLeftM_ #-}
