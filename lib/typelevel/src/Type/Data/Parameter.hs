{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Parameter where

import Prologue



----------------------------------
-- === Parameter management === --
----------------------------------

-- | These utilities provide tools for data parameter modification, like
--   discovering type parameter by its kind or replacing it with new one.


-- === Definition === --

type family DefaultByKind (k :: Type) :: k
type family GetByKind     (k :: Type) (t :: s) :: k where
    GetByKind k (t a) = a
    GetByKind k (t _) = GetByKind k t
    GetByKind k _     = DefaultByKind k

type family SetByKind (k :: Type) (val :: k) (t :: s) :: s where
    SetByKind k v (t _) = t v
    SetByKind k v (t a) = SetByKind k v t a

type family ReplaceByKind (k :: Type) (val :: k) (t :: s) :: s where
    ReplaceByKind k v (t _) = t v
    ReplaceByKind k v (t a) = ReplaceByKind k v t a
    ReplaceByKind _ _ t     = t


-- === Utils === --

setByKind :: ∀ k v a. a -> SetByKind k v a
setByKind = unsafeCoerce
{-# INLINE setByKind #-}

replaceByKind :: ∀ k v a. a -> ReplaceByKind k v a
replaceByKind = unsafeCoerce
{-# INLINE replaceByKind #-}

