module Data.Convert.Instances.Base where

import Data.Convert.Class

instance {-# OVERLAPPABLE #-} (Convertible a b, Functor f) => Convertible (f a) (f b) where
    convert = fmap convert ; {-# INLINE convert #-}

instance Convertible ()                  [t] where convert _                            = [] ; {-# INLINE convert #-}
instance Convertible (t,t)               [t] where convert (t1,t2)                      = [t1,t2] ; {-# INLINE convert #-}
instance Convertible (t,t,t)             [t] where convert (t1,t2,t3)                   = [t1,t2,t3] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t)           [t] where convert (t1,t2,t3,t4)                = [t1,t2,t3,t4] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t)         [t] where convert (t1,t2,t3,t4,t5)             = [t1,t2,t3,t4,t5] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t)       [t] where convert (t1,t2,t3,t4,t5,t6)          = [t1,t2,t3,t4,t5,t6] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t)     [t] where convert (t1,t2,t3,t4,t5,t6,t7)       = [t1,t2,t3,t4,t5,t6,t7] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t,t)   [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8)    = [t1,t2,t3,t4,t5,t6,t7,t8] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t,t,t) [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8,t9) = [t1,t2,t3,t4,t5,t6,t7,t8,t9] ; {-# INLINE convert #-}
