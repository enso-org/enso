module Prologue.Control.Error (module Prologue.Control.Error, module X) where

import Prelude hiding (take, drop)
import Control.Monad
import Control.Error.Safe as X
    ( tryTail, tryInit, tryHead, tryLast, tryMinimum, tryMaximum, tryFoldr1, tryFoldl1, tryFoldl1', tryRead, tryAssert, tryAssert, tryJust, tryRight
    , tailErr, initErr, headErr, lastErr, minimumErr, maximumErr, foldr1Err, foldl1Err, foldl1Err', readErr, assertErr, assertErr, justErr
    )
import Control.Error.Util as X ( hush, hushT, note, noteT
                               , isJustT, isNothingT, nothing, just, isLeftT, isRightT
                               )

import Data.Maybe
import qualified Prelude            as P
import qualified Data.List          as P
import qualified Control.Error.Safe as S
import qualified Data.List          as List


unsafeTail    :: [a] -> [a]
unsafeInit    :: [a] -> [a]
unsafeHead    :: [a] -> a
unsafeLast    :: [a] -> a
unsafeFoldr1  :: (a -> a -> a) -> [a] -> a
unsafeFoldl1  :: (a -> a -> a) -> [a] -> a
unsafeFoldl1' :: (a -> a -> a) -> [a] -> a
unsafeRead    :: forall a. Read a => String -> a
unsafeTail    = P.tail    ; {-# INLINE unsafeTail    #-}
unsafeInit    = P.init    ; {-# INLINE unsafeInit    #-}
unsafeHead    = P.head    ; {-# INLINE unsafeHead    #-}
unsafeLast    = P.last    ; {-# INLINE unsafeLast    #-}
unsafeFoldr1  = P.foldr1  ; {-# INLINE unsafeFoldr1  #-}
unsafeFoldl1  = P.foldl1  ; {-# INLINE unsafeFoldl1  #-}
unsafeFoldl1' = P.foldl1' ; {-# INLINE unsafeFoldl1' #-}
unsafeRead    = P.read    ; {-# INLINE unsafeRead    #-}

tail    :: MonadPlus m => [a] -> m [a]
init    :: MonadPlus m => [a] -> m [a]
head    :: MonadPlus m => [a] -> m a
last    :: MonadPlus m => [a] -> m a
foldr1  :: MonadPlus m => (a -> a -> a) -> [a] -> m a
foldl1  :: MonadPlus m => (a -> a -> a) -> [a] -> m a
foldl1' :: MonadPlus m => (a -> a -> a) -> [a] -> m a
read    :: MonadPlus m => Read a => String -> m a
tail    = S.tailZ    ; {-# INLINE tail    #-}
init    = S.initZ    ; {-# INLINE init    #-}
head    = S.headZ    ; {-# INLINE head    #-}
last    = S.lastZ    ; {-# INLINE last    #-}
foldr1  = S.foldr1Z  ; {-# INLINE foldr1  #-}
foldl1  = S.foldl1Z  ; {-# INLINE foldl1  #-}
foldl1' = S.foldl1Z' ; {-# INLINE foldl1' #-}
read    = S.readZ    ; {-# INLINE read    #-}

takeExactly :: MonadPlus m => Int -> [a] -> m [a]
takeExactly i a = if
    | i < 0     -> mzero
    | i == 0    -> pure mempty
    | otherwise -> case a of
        (l:ls) -> (l :) <$> takeExactly (i - 1) ls
        []     -> mzero
{-# NOINLINE takeExactly #-}

dropExactly :: MonadPlus m => Int -> [a] -> m [a]
dropExactly i a = if
    | i < 0     -> mzero
    | i == 0    -> pure a
    | otherwise -> case a of
        (_:ls) -> dropExactly (i - 1) ls
        []     -> mzero
{-# NOINLINE dropExactly #-}

splitAtExactly :: MonadPlus m => Int -> [a] -> m ([a], [a])
splitAtExactly i a = (,) <$> takeExactly i a <*> dropExactly i a ; {-# INLINE splitAtExactly #-}

-- FIXME[WD]: Are we sure this function belongs to Prologue?
splitHead :: [a] -> (Maybe a, [a])
splitHead ps = (val, rest) where
    pair = List.uncons ps
    val  = fmap fst pair
    rest = fromMaybe mempty $ fmap snd pair
{-# INLINE splitHead #-}

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero pure ; {-# INLINE hoistMaybe #-}
