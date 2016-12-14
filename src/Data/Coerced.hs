module Data.Coerced where

import Control.Lens  (Iso', Iso, iso)
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce   (Coercible, coerce)


-- safe
coerced :: Coercible a b => Iso' a b
coerced = iso coerce coerce ; {-# INLINE coerced #-}

unsafeCoerced :: Iso s t a b
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
