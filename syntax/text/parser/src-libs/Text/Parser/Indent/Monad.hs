module Text.Parser.Indent.Monad where

import Prologue hiding (guard)

import           Control.Monad.State.Dependent hiding (get, with)
import qualified Control.Monad.State.Dependent as State
import           Data.Text.Position
import           Text.Parser.Indent.Class (Indent, level, stack)
import qualified Text.Parser.Indent.Class as Indent
import qualified Control.Monad as Monad

-- === Indent management === --

get :: MonadGetter Indent m => m Delta
get = view level <$> State.get @Indent

pushCurrent :: (MonadState Indent m, MonadGetter Position m) => m ()
pushCurrent = push =<< getColumn

push :: MonadState Indent m => Delta -> m ()
push = modify_ @Indent . Indent.push

pop :: MonadState Indent m => m Delta
pop = modify @Indent Indent.pop

with :: MonadState Indent m => Delta -> m a -> m a
with d m = push d *> m <* pop

withCurrent :: (MonadState Indent m, MonadGetter Position m) => m a -> m a
withCurrent m = pushCurrent *> m <* pop

withRoot :: MonadState Indent m => m a -> m a
withRoot = with 0

checkIndentRef :: MonadGetters '[Indent, Position] m => (Delta -> Delta -> a) -> m a
checkIndentRef f = f <$> getColumn <*> get

checkIndent :: MonadGetters '[Indent, Position] m => (Delta -> a) -> m a
checkIndent f = checkIndentRef $ f .: (-)

checkIndented :: MonadGetters '[Indent, Position] m => (Ordering -> a) -> m a
checkIndented f = checkIndentRef $ f .: compare

-- TODO[WD]: make it safe
guard :: MonadGetters '[Indent, Position] m => (Ordering -> Bool) -> String -> m ()
guard ord err = flip when (Monad.fail err) . not =<< checkIndented ord

indented, indentedOrEq, indentedEq :: MonadGetters '[Indent, Position] m => m ()
indented     = guard (== GT) "Expected indentation"
indentedOrEq = guard (/= LT) "Expected indentation"
indentedEq   = guard (== EQ) "Indentation does not match the previous one"

indentation :: MonadGetters '[Indent, Position] m => m Delta
indentation = (-) <$> getColumn <*> get
