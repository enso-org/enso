{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Name.FastString (module X) where

import Prelude.Luna
import FastString   as X


-- === FastString Instances === --

-- Normal Form
instance NFData FastString where rnf = const ()

-- Read
instance Read FastString where readPrec = fromString <$> readPrec ; {-# INLINE readPrec #-}

-- Monoid
instance Monoid FastString where
    mempty      = ""             ; {-# INLINE mempty  #-}
    mappend a b = concatFS [a,b] ; {-# INLINE mappend #-}

-- Construction
instance IsString FastString where fromString = convert ; {-# INLINE fromString #-}
instance ToString FastString where toString   = convert ; {-# INLINE toString   #-}

-- Repr
instance Repr s String => Repr s FastString where repr = repr ∘ toString ; {-# INLINE repr #-}

-- Conversions
instance Convertible FastString String     where convert = unpackFS ; {-# INLINE convert #-}
instance Convertible String     FastString where convert = fsLit    ; {-# INLINE convert #-}
