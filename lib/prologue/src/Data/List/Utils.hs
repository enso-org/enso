module Data.List.Utils where

import Prelude

class SplitOn s a where
    splitOn :: s -> a -> [a]

instance Eq a => SplitOn a [a] where
    splitOn t = reverse . fmap reverse . go [] [] where
        go done current = \case
            []     -> (if null current then id else (current:)) $ done
            (a:as) -> if a == t then go (current:done) [] as
                                else go done (a:current)  as