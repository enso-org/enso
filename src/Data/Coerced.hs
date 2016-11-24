module Data.Coerced (
      coerced
    , unsafeCoerced
    ) where

import Control.Lens (Iso', iso, coerced)
import Unsafe.Coerce (unsafeCoerce)


unsafeCoerced :: Iso' a b
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
