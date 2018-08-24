{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monadic / applicative parser abstractions.

module Data.Parser where

import Prelude hiding (takeWhile, (.))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Foldable hiding (toList)
import GHC.Exts (IsList, Item, toList, fromList)
import Data.Convert
import Data.Functor.Utils

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


--------------------------
-- === FiniteParser === --
--------------------------

class FiniteParser m where
    endOfInput :: m ()

instance {-# OVERLAPPABLE #-} (FiniteParser m, MonadTrans t, Monad m) => FiniteParser (t m) where
    endOfInput = lift endOfInput ; {-# INLINE endOfInput #-}


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
    peekToken  :: m (Token m)
    peekToken' :: m (Maybe (Token m))

    default takeWhile  :: MonadPlus m   => (Token m -> Bool) -> m (Tokens m)
    default takeWhile1 :: MonadPlus m   => (Token m -> Bool) -> m (Tokens m)
    default token_     :: Eq (Token m)  => Token  m -> m ()
    default tokens_    :: Eq (Token m)  => Tokens m -> m ()
    default peekToken' :: Alternative m => m (Maybe (Token m))
    takeWhile  f = fromList <$> many'  (satisfy f)     ; {-# INLINE takeWhile  #-}
    takeWhile1 f = fromList <$> many1' (satisfy f)     ; {-# INLINE takeWhile1 #-}
    anyToken     = satisfy $ const True                ; {-# INLINE anyToken   #-}
    token_     t = void $ satisfy (== t)               ; {-# INLINE token_     #-}
    tokens_    t = traverse_ token_ (toList t)         ; {-# INLINE tokens_    #-}
    peekToken'   = option Nothing $ Just <$> peekToken ; {-# INLINE peekToken' #-}

instance {-# OVERLAPPABLE #-} (TokenParserCtx (t m), TokenParser m, TokenTrans t m, MonadTrans t, Monad m)
      => TokenParser (t m) where
    satisfy    = lift . satisfy    ; {-# INLINE satisfy    #-}
    takeWhile  = lift . takeWhile  ; {-# INLINE takeWhile  #-}
    takeWhile1 = lift . takeWhile1 ; {-# INLINE takeWhile1 #-}
    anyToken   = lift anyToken     ; {-# INLINE anyToken   #-}
    token_     = lift . token_     ; {-# INLINE token_     #-}
    tokens_    = lift . tokens_    ; {-# INLINE tokens_    #-}
    peekToken  = lift peekToken    ; {-# INLINE peekToken  #-}
    peekToken' = lift peekToken'   ; {-# INLINE peekToken' #-}


-- === Aliases === --

type CharParser m = (TokenParser m, Token m ~ Char, Convertible' Char (Tokens m), Convertible' Char (Token m))


-- === Utils === --

token  :: TokenParser m => Token  m -> m (Token  m)
tokens :: TokenParser m => Tokens m -> m (Tokens m)
token  t = t <$ token_  t ; {-# INLINE token  #-}
tokens t = t <$ tokens_ t ; {-# INLINE tokens #-}

dropToken :: TokenParser m => m ()
dropToken = void anyToken ; {-# INLINE dropToken #-}

takeTill  :: TokenParser m => (Token m -> Bool) -> m (Tokens m)
takeTill1 :: TokenParser m => (Token m -> Bool) -> m (Tokens m)
takeTill  f = takeWhile  (not . f) ; {-# INLINE takeTill  #-}
takeTill1 f = takeWhile1 (not . f) ; {-# INLINE takeTill1 #-}


-- === Combinators === --

option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x ; {-# INLINE option #-}

option_ :: Alternative f => f a -> f ()
option_ p = void p <|> pure () ; {-# INLINE option_ #-}

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty ; {-# INLINE choice #-}

many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p where
    many_p = some_p `mplus` return []
    some_p = liftM2' (:) p many_p
{-# INLINE many' #-}

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p) ; {-# INLINE many1 #-}

many1' :: MonadPlus m => m a -> m [a]
many1' p = liftM2' (:) p (many' p) ; {-# INLINE many1' #-}

takeMany :: (TokenParser m, Eq (Token m)) => Token m -> m (Tokens m)
takeMany = takeWhile . (==) ; {-# INLINE takeMany #-}

takeMany1 :: (TokenParser m, Eq (Token m)) => Token m -> m (Tokens m)
takeMany1 = takeWhile1 . (==) ; {-# INLINE takeMany1 #-}

count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p) ; {-# INLINE count #-}



-----------------------------
-- === BacktrackParser === --
-----------------------------

-- === Definition === --

class BacktrackParser m where
    try :: forall a. m a -> m a


-- === Combinators === --

notFollowedBy :: (BacktrackParser m, Alternative m, Monad m) => m a -> m ()
notFollowedBy p = option () $ try p >> fail "" ; {-# INLINE notFollowedBy #-}


-- === Parsing utils === --

takeLine :: CharParser m => m (Tokens m)
takeLine = takeWhile (\c -> c /= '\n' && c /= '\r') ; {-# INLINE takeLine #-}

newline :: (CharParser m, Alternative m) => m (Tokens m)
newline = escape_n <|> escape_rn where
    escape_n  = convert' <$> token '\n'
    escape_rn = token '\r' <**> option convert' ((\b a -> fromList [a,b]) <$> token '\n')
{-# INLINE newline #-}



---------------------------
-- === PartialParser === --
---------------------------

type family BaseMonad (m :: * -> *) :: * -> *

type family PartialResult (m :: * -> *) = (r :: * -> *) | r -> m
type family Result        (m :: * -> *) = (r :: * -> *) | r -> m

type  PartialParser' m = (PartialParser m, BaseMonad m ~ Identity)
class PartialParser  m where
    parsePartialT :: forall a.               m a -> Tokens m -> BaseMonad m (PartialResult m a)
    feedPartialT  :: forall a. PartialResult m a -> Tokens m -> BaseMonad m (PartialResult m a)
    closePartialT :: forall a. PartialResult m a             -> BaseMonad m        (Result m a)


-- === Utils === --

parsePartial :: PartialParser' m =>               m a -> Tokens m -> PartialResult m a
feedPartial  :: PartialParser' m => PartialResult m a -> Tokens m -> PartialResult m a
closePartial :: PartialParser' m => PartialResult m a -> Result m a
parsePartial = runIdentity .: parsePartialT ; {-# INLINE parsePartial #-}
feedPartial  = runIdentity .: feedPartialT  ; {-# INLINE feedPartial  #-}
closePartial = runIdentity .  closePartialT ; {-# INLINE closePartial #-}
