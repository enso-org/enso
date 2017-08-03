{-# LANGUAGE UndecidableInstances #-}

module Data.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Foldable hiding (toList)
import GHC.Exts (IsList, Item, toList, fromList)

-- General utils -- to refactor
import qualified Control.Monad.State.Layered as Layered

type family Token  (m :: * -> *)
type family Tokens (m :: * -> *)

type instance Token  (Layered.StateT s m) = Token  m
type instance Tokens (Layered.StateT s m) = Tokens m

type TokenInvariants m   = (Item (Tokens m) ~ Token m, IsList (Tokens m))
type TokenTrans      t m = (Token (t m) ~ Token m, Tokens (t m) ~ Tokens m)



-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y  <- b
  return (f x y)
{-# INLINE liftM2' #-}


-------------------------
-- === TokenParser === --
-------------------------

-- === Definition === --

type  TokenParserCtx m = (Applicative m, TokenInvariants m)
class TokenParserCtx m => TokenParser m where
    satisfy    :: (Token m -> Bool) -> m (Token  m)
    takeWhile  :: (Token m -> Bool) -> m (Tokens m)
    takeWhile1 :: (Token m -> Bool) -> m (Tokens m)
    anyToken   :: m (Token m)
    token_     :: Token  m -> m ()
    tokens_    :: Tokens m -> m ()

    default takeWhile  :: MonadPlus m  => (Token m -> Bool) -> m (Tokens m)
    default takeWhile1 :: MonadPlus m  => (Token m -> Bool) -> m (Tokens m)
    default token_     :: Eq (Token m) => Token  m -> m ()
    default tokens_    :: Eq (Token m) => Tokens m -> m ()
    takeWhile  f = fromList <$> many'  (satisfy f) ; {-# INLINE takeWhile  #-}
    takeWhile1 f = fromList <$> many1' (satisfy f) ; {-# INLINE takeWhile1 #-}
    anyToken     = satisfy $ const True            ; {-# INLINE anyToken   #-}
    token_     t = void $ satisfy (== t)           ; {-# INLINE token_     #-}
    tokens_    t = traverse_ token_ (toList t)     ; {-# INLINE tokens_    #-}

instance (TokenParserCtx (t m), TokenParser m, TokenTrans t m, MonadTrans t, Monad m)
      => TokenParser (t m) where
    satisfy    = lift . satisfy    ; {-# INLINE satisfy    #-}
    takeWhile  = lift . takeWhile  ; {-# INLINE takeWhile  #-}
    takeWhile1 = lift . takeWhile1 ; {-# INLINE takeWhile1 #-}
    anyToken   = lift anyToken     ; {-# INLINE anyToken   #-}
    token_     = lift . token_     ; {-# INLINE token_     #-}
    tokens_    = lift . tokens_    ; {-# INLINE tokens_    #-}


-- === Utils === --

token  :: TokenParser m => Token  m -> m (Token  m)
tokens :: TokenParser m => Tokens m -> m (Tokens m)
token  t = t <$ token_  t ; {-# INLINE token  #-}
tokens t = t <$ tokens_ t ; {-# INLINE tokens #-}


-- === Combinators === --

option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x ; {-# INLINE option #-}

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty ; {-# INLINE choice #-}

many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p where
    many_p = some_p `mplus` return []
    some_p = liftM2' (:) p many_p
{-# INLINE many' #-}

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p) ; {-# INLINE many1 #-}

many1' :: (MonadPlus m) => m a -> m [a]
many1' p = liftM2' (:) p (many' p) ; {-# INLINE many1' #-}
