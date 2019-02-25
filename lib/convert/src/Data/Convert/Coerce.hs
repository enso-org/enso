module Data.Convert.Coerce (module Data.Convert.Coerce, module X) where

import Control.Lens     (Iso, iso)
import Unsafe.Coerce    (unsafeCoerce)
import Control.Lens.Iso as X (coerced)

unsafeCoerced :: Iso s t a b
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
