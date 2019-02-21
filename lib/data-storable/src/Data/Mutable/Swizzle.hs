{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Mutable.Swizzle where



-----------------------
-- === Swizzling === --
-----------------------

-- === Definition === --

class Swizzling a m where
    type family Unswizzled a
    type Unswizzled a = a

    unswizzle :: a -> m (Unswizzled a)
    swizzle   :: Unswizzled a -> m a

