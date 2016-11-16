module Data.Coerced where

import Control.Lens
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce   (coerce, Coercible)


-- safe
coerced :: Coercible a b => Iso' a b
coerced = iso coerce coerce ; {-# INLINE coerced #-}

unsafeCoerced :: Iso' a b
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
