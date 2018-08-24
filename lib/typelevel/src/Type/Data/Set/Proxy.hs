{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Set.Proxy where

import           Data.Proxy
import           Prelude
import qualified Type.Data.Set as Set


-----------------
-- === Set === --
-----------------

-- === Definition === --

type Set k = Proxy ('Set.Set k)

-- === Operations === --

type family Insert k s where Insert k (Proxy s) = Proxy (Set.Insert k s)
type family Member k s where Member k (Proxy s) = Proxy (Set.Member k s)

