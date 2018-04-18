module Luna.Syntax.Text.Parser.Reserved where

import Prologue hiding (SomeSymbol, Symbol, lookup)

import qualified Control.Monad.State.Layered as State
import qualified Data.Set                    as Set

import Data.Set                           (Set)
import Luna.Syntax.Text.Parser.Pass.Class (Symbol)



----------------------
-- === Reserved === --
----------------------

-- === Definition === --

data Reserved = Reserved
    { _global :: !(Set Symbol) -- | Reserved globally
    , _local  :: !(Set Symbol) -- | Reserved only for a particular depth
    } deriving (Show)
makeLenses ''Reserved

type MonadReserved m = State.Monad Reserved m


-- === API === --

with     ::  MonadReserved m              =>   Symbol -> m a -> m a
withMany :: (MonadReserved m, Foldable f) => f Symbol -> m a -> m a
with n   = State.withModified @Reserved (global %~ Set.insert n) ; {-# INLINE with     #-}
withMany = flip $ foldl (flip with)                              ; {-# INLINE withMany #-}

withLocal     ::  MonadReserved m              =>   Symbol -> m a -> m a
withoutLocal  ::  MonadReserved m              =>   Symbol -> m a -> m a
withLocalMany :: (MonadReserved m, Foldable f) => f Symbol -> m a -> m a
withLocal    n = State.withModified @Reserved (local %~ Set.insert n) ; {-# INLINE withLocal     #-}
withoutLocal n = State.withModified @Reserved (local %~ Set.delete n) ; {-# INLINE withoutLocal  #-}
withLocalMany  = flip $ foldl (flip withLocal)                           ; {-# INLINE withLocalMany #-}

withNewLocal :: MonadReserved m => m a -> m a
withNewLocal = State.withModified @Reserved $ local .~ mempty ; {-# INLINE withNewLocal #-}

check :: MonadReserved m => Symbol -> m Bool
check n = flip lookup n <$> State.get @Reserved ; {-# INLINE check #-}

lookup :: Reserved -> Symbol -> Bool
lookup s n = (Set.member n $ view local s) || (Set.member n $ view global s) ; {-# INLINE lookup #-}


-- === Instances === --

instance Default Reserved where def = Reserved def def ; {-# INLINE def #-}
