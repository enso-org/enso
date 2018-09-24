{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Symbol.Operator.Prec where

import Prologue

import Control.Monad.State.Layered (StateT)
import Language.Symbol.Label       (HasLabel, LabelOf, label)



------------------------
-- === Precedence === --
------------------------

-- === Precedence relations === --

type RelManager label m = (RelReader label m, RelWriter label m)

class Monad m => RelReader label m where
    readRelLabel :: label -> label -> m (Maybe Ordering)
    default readRelLabel :: (m ~ t n, RelReader label n, MonadTrans t) => label -> label -> m (Maybe Ordering)
    readRelLabel = lift .: readRelLabel

class Monad m => RelWriter label m where
    writeRelLabel :: Ordering -> label -> label -> m ()
    default writeRelLabel :: (m ~ t n, RelWriter label n, MonadTrans t) => Ordering -> label -> label -> m ()
    writeRelLabel = lift .:. writeRelLabel


-- === Utils === --

readRel :: (RelReader (LabelOf a) m, HasLabel a, HasLabel b, LabelOf a ~ LabelOf b)
    => a -> b -> m (Maybe Ordering)
readRel a b = readRelLabel (a ^. label) (b ^. label)

writeRel :: (RelWriter (LabelOf a) m, HasLabel a, HasLabel b, LabelOf a ~ LabelOf b)
    => Ordering -> a -> b -> m ()
writeRel ord a b = writeRelLabel ord (a ^. label) (b ^. label)


-- === Limited precedence === --

type ExtremumWriter label m = (MaxPrecWriter label m, MinPrecWriter label m)

class Monad m => MaxPrecWriter label m where
    writeMax :: label -> m ()
    default writeMax :: (m ~ t n, MaxPrecWriter label n, MonadTrans t) => label -> m ()
    writeMax = lift . writeMax

class Monad m => MinPrecWriter label m where
    writeMin :: label -> m ()
    default writeMin  :: (m ~ t n, MinPrecWriter label n, MonadTrans t) => label -> m ()
    writeMin = lift . writeMin


data Bound a
    = Max
    | Min
    | Normal a
    deriving (Show, Eq)


-- === Instances === --

instance {-# OVERLAPPABLE #-} RelReader label m => RelReader label (StateT s m)
instance {-# OVERLAPPABLE #-} RelWriter label m => RelWriter label (StateT s m)
instance {-# OVERLAPPABLE #-} RelReader label m => RelReader label (IdentityT m)
instance {-# OVERLAPPABLE #-} RelWriter label m => RelWriter label (IdentityT m)
