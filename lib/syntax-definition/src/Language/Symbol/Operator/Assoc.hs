{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Symbol.Operator.Assoc where

import Prologue hiding (read)

import Control.Monad.State.Layered (StateT)
import Language.Symbol.Label


-------------------------
-- === Association === --
-------------------------

-- === Definition === --

data Assoc = Left | Right | None deriving (Show, Eq)


-- === Monadic interface === --

class Monad m => Reader label m where
    readLabel :: label -> m Assoc
    default readLabel :: (m ~ t n, Reader label n, MonadTrans t) => label -> m Assoc
    readLabel = lift . readLabel

class Monad m => Writer label m where
    writeLabel :: Assoc -> label -> m ()
    default writeLabel :: (m ~ t n, Writer label n, MonadTrans t) => Assoc -> label -> m ()
    writeLabel = lift .: writeLabel


read :: (Reader (LabelOf a) m, HasLabel a) => a -> m Assoc
read = readLabel . view label

write :: (Writer (LabelOf a) m, HasLabel a) => Assoc -> a -> m ()
write asc = writeLabel asc . view label


-- === Instances === --

instance {-# OVERLAPPABLE #-} Reader label m => Reader label (StateT s m)
instance {-# OVERLAPPABLE #-} Writer label m => Writer label (StateT s m)
instance {-# OVERLAPPABLE #-} Reader label m => Reader label (IdentityT m)
instance {-# OVERLAPPABLE #-} Writer label m => Writer label (IdentityT m)
