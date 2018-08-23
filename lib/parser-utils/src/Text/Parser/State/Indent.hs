module Text.Parser.State.Indent where

import Prologue hiding (guard)

import qualified Control.Monad               as Monad
import qualified Control.Monad.State.Layered as State
import qualified Data.Text.Position          as Position

import Control.Monad.State.Layered (StateT)
import Data.Text.Position          (Delta, Position)



--------------------------
-- === Indent State === --
--------------------------

-- === Definition === --

newtype Indent = Indent
    { _level :: Delta
    } deriving (Default, Show, Mempty)
makeLenses ''Indent


-- === Running === --

eval :: Monad m => StateT Indent m a -> m a
eval = flip (State.evalT @Indent) mempty
{-# INLINE eval #-}


-- === Pure API === --

put' :: Delta -> Indent -> Indent
put' d i = i & level .~ d
{-# INLINE put' #-}


-- === Monadic API === --

put :: State.Monad Indent m => Delta -> m ()
put = State.modify_ @Indent . put'
{-# INLINE put #-}

get :: State.Getter Indent m => m Delta
get = view level <$> State.get @Indent
{-# INLINE get #-}

with :: State.Monad Indent m => Delta -> m a -> m a
with = \ind m -> do
    old <- get
    put ind
    out <- m
    put old
    pure out
{-# INLINE with #-}

withCurrent :: (State.Monad Indent m, State.Getter Position m) => m a -> m a
withCurrent = \m -> do
    col <- Position.getColumn
    with col m
{-# INLINE withCurrent #-}

withRoot :: State.Monad Indent m => m a -> m a
withRoot = with $ mempty ^. level
{-# INLINE withRoot #-}

checkIndentRef :: State.Getters '[Indent, Position] m
    => (Delta -> Delta -> a) -> m a
checkIndentRef = \f -> f <$> Position.getColumn <*> get
{-# INLINE checkIndentRef #-}

checkIndent :: State.Getters '[Indent, Position] m => (Delta -> a) -> m a
checkIndent = \f -> checkIndentRef $ f .: (-)
{-# INLINE checkIndent #-}

checkIndented :: State.Getters '[Indent, Position] m => (Ordering -> a) -> m a
checkIndented = \f -> checkIndentRef $ f .: compare
{-# INLINE checkIndented #-}

guard :: State.Getters '[Indent, Position] m
    => (Ordering -> Bool) -> String -> m ()
guard = \ord err -> flip when (Monad.fail err) . not =<< checkIndented ord
{-# INLINE guard #-}

indented, indentedOrEq, indentedEq
    :: State.Getters '[Indent, Position] m => m ()
indented     = guard (== GT) "Expected indentation"
indentedOrEq = guard (/= LT) "Expected indentation"
indentedEq   = guard (== EQ) "Indentation does not match the previous one"
{-# INLINE indented     #-}
{-# INLINE indentedOrEq #-}
{-# INLINE indentedEq   #-}

indentation :: State.Getters '[Indent, Position] m => m Delta
indentation = (-) <$> Position.getColumn <*> get
{-# INLINE indentation #-}
