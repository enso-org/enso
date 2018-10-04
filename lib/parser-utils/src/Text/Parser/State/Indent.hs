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

guard :: State.Getters '[Indent, Position] m
    => (Delta -> Bool) -> String -> m Delta
guard = \test err -> do
    ref <- get
    col <- Position.getColumn
    let diff = col - ref
        ok   = test diff
    if ok then pure diff
          else Monad.fail err
{-# INLINE guard #-}

expectedInentationError  :: String
indentationMismatchError :: String
expectedInentationError  = "Expected indentation"
indentationMismatchError = "Indentation does not match the previous one"
{-# INLINE expectedInentationError  #-}
{-# INLINE indentationMismatchError #-}

indented, indentedOrEq, indentedEq
    :: State.Getters '[Indent, Position] m => m Delta
indented     = guard (>  0) expectedInentationError
indentedOrEq = guard (>= 0) expectedInentationError
indentedEq   = guard (== 0) indentationMismatchError
{-# INLINE indented     #-}
{-# INLINE indentedOrEq #-}
{-# INLINE indentedEq   #-}

indentation :: State.Getters '[Indent, Position] m => m Delta
indentation = (-) <$> Position.getColumn <*> get
{-# INLINE indentation #-}
