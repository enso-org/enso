module Data.Coerced (
      coerced
    , unsafeCoerced
    ) where

import Control.Lens (Iso', iso, coerced)
import Unsafe.Coerce (unsafeCoerce)


-- safe
coerced :: Coercible a b => Iso' a b
coerced = iso coerce coerce ; {-# INLINE coerced #-}

unsafeCoerced :: Iso s t a b
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
