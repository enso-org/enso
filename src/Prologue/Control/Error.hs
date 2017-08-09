module Prologue.Control.Error (module Prologue.Control.Error, module X) where

import Prelude
import Control.Monad
import Control.Error.Safe as X
    ( tryTail, tryInit, tryHead, tryLast, tryMinimum, tryMaximum, tryFoldr1, tryFoldl1, tryFoldl1', tryRead, tryAssert, tryAssert, tryJust, tryRight
    , tailZ  , initZ  , headZ  , lastZ  , minimumZ  , maximumZ  , foldr1Z  , foldl1Z  , foldl1Z'  , readZ  , assertZ  , assertZ  , justZ  , rightZ
    , tailErr, initErr, headErr, lastErr, minimumErr, maximumErr, foldr1Err, foldl1Err, foldl1Err', readErr, assertErr, assertErr, justErr
    )
import Control.Error.Util as X ( hush, hushT, note, noteT
                               , isJustT, isNothingT, nothing, just, isLeftT, isRightT
                               )

import Data.Maybe
import qualified Data.List as List


takeZ :: MonadPlus m => Int -> [a] -> m [a]
takeZ i a = if
    | i < 0     -> mzero
    | i == 0    -> pure mempty
    | otherwise -> case a of
        (l:ls) -> (l :) <$> takeZ (i - 1) ls
        []     -> mzero
{-# NOINLINE takeZ #-}

dropZ :: MonadPlus m => Int -> [a] -> m [a]
dropZ i a = if
    | i < 0     -> mzero
    | i == 0    -> pure a
    | otherwise -> case a of
        (l:ls) -> dropZ (i - 1) ls
        []     -> mzero
{-# NOINLINE dropZ #-}

splitAtZ :: MonadPlus m => Int -> [a] -> m ([a], [a])
splitAtZ i a = (,) <$> takeZ i a <*> dropZ i a ; {-# INLINE splitAtZ #-}

-- FIXME[WD]: Are we sure this function belongs to Prologue?
splitHead :: [a] -> (Maybe a, [a])
splitHead ps = (val, rest) where
    pair = List.uncons ps
    val  = fmap fst pair
    rest = fromMaybe mempty $ fmap snd pair
{-# INLINE splitHead #-}

hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero pure ; {-# INLINE hoistMaybe #-}
