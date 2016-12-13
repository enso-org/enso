module Data.Coerced where

<<<<<<< HEAD
import Control.Lens  (Iso', Iso, iso)
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce   (Coercible, coerce)
=======
import Control.Lens
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce   (coerce, Coercible)
>>>>>>> parent of 3bb8850... Make it build


-- safe
coerced :: Coercible a b => Iso' a b
coerced = iso coerce coerce ; {-# INLINE coerced #-}

<<<<<<< HEAD
unsafeCoerced :: Iso s t a b
=======
unsafeCoerced :: Iso' a b
>>>>>>> parent of 3bb8850... Make it build
unsafeCoerced = iso unsafeCoerce unsafeCoerce ; {-# INLINE unsafeCoerced #-}
