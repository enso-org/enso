module Data.Parser.Instances.Attoparsec where

import Prelude
import Data.Parser
import Control.Monad.Identity
import qualified Data.Attoparsec.Text as SText
import Data.Attoparsec.Internal.Types (Parser, IResult)
import qualified Data.Text as Strict
import Control.Monad (void)
import Data.Functor.Utils


-- === Strict.Text === --

type instance Token  (Parser Strict.Text) = Char
type instance Tokens (Parser t) = t

instance TokenParser (Parser Strict.Text) where
    satisfy    = SText.satisfy       ; {-# INLINE satisfy    #-}
    takeWhile  = SText.takeWhile     ; {-# INLINE takeWhile  #-}
    takeWhile1 = SText.takeWhile1    ; {-# INLINE takeWhile1 #-}
    anyToken   = SText.anyChar       ; {-# INLINE anyToken   #-}
    token_     = void . SText.char   ; {-# INLINE token_     #-}
    tokens_    = void . SText.string ; {-# INLINE tokens_    #-}
    peekToken  = SText.peekChar'     ; {-# INLINE peekToken  #-}
    peekToken' = SText.peekChar      ; {-# INLINE peekToken' #-}

instance BacktrackParser (Parser a) where
    try = id ; {-# INLINE try #-}


type instance BaseMonad (Parser t) = Identity

type instance PartialResult (Parser t) = IResult t
type instance Result        (Parser t) = IResult t

instance PartialParser (Parser Strict.Text) where
    parsePartialT = pure .: SText.parse      ; {-# INLINE parsePartialT #-}
    feedPartialT  = pure .: SText.feed       ; {-# INLINE feedPartialT  #-}
    closePartialT = flip feedPartialT mempty ; {-# INLINE closePartialT #-}
--
-- type  PartialParser  m = (PartialParserT m, BaseMonad m ~ Identity)
-- class PartialParserT m where
--     parsePartialT :: forall a. m a -> BaseMonad m (PartialResult m a)
--     feedPartialT  :: forall a. PartialResult m a -> Tokens m -> BaseMonad m (PartialResult m a)
--     closePartialT :: forall a. PartialResult m a -> BaseMonad m (Result m a)

-- feed :: Monoid i => IResult i r -> i -> IResult i r
