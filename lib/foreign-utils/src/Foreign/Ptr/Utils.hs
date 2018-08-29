{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Foreign.Ptr.Utils where

import Prologue

import Foreign (Ptr)

type SomePtr = Ptr ()


-- | Function converting the c-like Int-encoded boolean to a Haskell one.
--   Exists just for the purpose of pinning-down this simple logic.
--   Note: it's lifted to IO so that it composes nicely with IO functions.
fromCBool :: MonadIO m => Int -> m Bool
fromCBool = pure . (/= 0)
{-# INLINE fromCBool #-}

