module Data.Graph.Class where

import Prologue



-------------------
-- === Graph === --
-------------------

-- === Definition === --

newtype Graph tag m a = Graph (IdentityT m a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)
makeLenses ''Graph

type family Components      tag      :: [Type]
type family ComponentLayers tag comp :: [Type]


type family DiscoverGraphTag (m :: Type -> Type) :: Type -- where
    -- DiscoverGraphTag (Graph tag m) = tag
    -- DiscoverGraphTag (t m)         = DiscoverGraphTag m

type DiscoverComponents      m = Components (DiscoverGraphTag m)
type DiscoverComponentLayers m comp = ComponentLayers (DiscoverGraphTag m) comp



-- === API === --

run :: ∀ t m a. Graph t m a -> m a
run = runIdentityT . unwrap ; {-# INLINE run #-}


