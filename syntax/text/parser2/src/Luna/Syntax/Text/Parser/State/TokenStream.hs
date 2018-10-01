
module Luna.Syntax.Text.Parser.State.TokenStream where

import Prologue

import qualified Control.Monad.State.Layered              as State
import qualified Luna.Syntax.Text.Parser.Ast.Spanned as Spanned

import Control.Monad.State.Layered              (StateT)
import Luna.Syntax.Text.Parser.Ast.Spanned (Ast, Spanned)



-------------------------
-- === TokenStream === --
-------------------------

-- === Definition === --

newtype TokenStream = TokenStream [Spanned Ast] deriving (Default, Mempty, Show)
makeLenses ''TokenStream


-- === API === --

eval :: Monad m => StateT TokenStream m a -> m [Spanned Ast]
eval = fmap (reverse . unwrap) . State.execDefT
{-# INLINE eval #-}

add :: State.Monad TokenStream m => Spanned Ast -> m ()
add = \a -> State.modify_ @TokenStream $ wrapped %~ (a:)
{-# INLINE add #-}

lookupLastToken :: State.Getter TokenStream m => m (Maybe (Spanned Ast))
lookupLastToken = head . unwrap <$> State.get @TokenStream
{-# INLINE lookupLastToken #-}

lookupLastSymbol :: State.Getter TokenStream m => m (Maybe Ast)
lookupLastSymbol = Spanned.unsafeUnspan <<$>> lookupLastToken
{-# INLINE lookupLastSymbol #-}
