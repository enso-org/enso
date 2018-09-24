{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Text.Parser.Combinators where

import Control.Applicative as Applicative
import Control.Lens
import Control.Monad
import Data.List.NonEmpty
import Prelude


a <**?> f = a <**> option id f
f <?*>  a = option id f <*> a
f ?$    a = f a <|> pure a


option :: Alternative m => a -> m a -> m a
option = \def p -> p <|> pure def
{-# INLINE option #-}

option_ :: Alternative m => m a -> m ()
option_ = option () . void
{-# INLINE option_ #-}

optionMaybe :: Alternative m => m a -> m (Maybe a)
optionMaybe = option Nothing . fmap Just
{-# INLINE optionMaybe #-}

checkSuccess :: Alternative m => m a -> m Bool
checkSuccess p = option False (True <$ p)
{-# INLINE checkSuccess #-}


-- | Evaluates action `p` and passes its reult to function f.
--   If `p` failed, default result `def` is returned.
--   However if `p` succeeded and `f` failed, the whole computation fails as well.
tryBind :: (Monad m, Alternative m) => b -> m a -> (a -> m b) -> m b
tryBind def p f = join $ ($ f) <$> option (const $ pure def) ((&) <$> p)


many1 :: (Applicative m, Alternative m) => m a -> m (NonEmpty a)
many1 = \p -> (:|) <$> p <*> many p
{-# INLINE many1 #-}


many1' :: (Applicative m, Alternative m) => m a -> m [a]
many1' = \p -> convert <$> many1 p where
    convert (a :| as) = a : as
{-# INLINE many1' #-}

-- someAsList :: (Applicative m, Alternative m) => m a -> m [a]
-- someAsList = Applicative.some ; {-# INLINE someAsList #-}



