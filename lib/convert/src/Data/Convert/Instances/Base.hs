{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Convert.Instances.Base where

import Prelude
import Data.Convert.Class
import Control.Lens


-- === Tuples === --

instance Convertible ()                  [t] where convert _                            = [] ; {-# INLINE convert #-}
instance Convertible (t,t)               [t] where convert (t1,t2)                      = [t1,t2] ; {-# INLINE convert #-}
instance Convertible (t,t,t)             [t] where convert (t1,t2,t3)                   = [t1,t2,t3] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t)           [t] where convert (t1,t2,t3,t4)                = [t1,t2,t3,t4] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t)         [t] where convert (t1,t2,t3,t4,t5)             = [t1,t2,t3,t4,t5] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t)       [t] where convert (t1,t2,t3,t4,t5,t6)          = [t1,t2,t3,t4,t5,t6] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t)     [t] where convert (t1,t2,t3,t4,t5,t6,t7)       = [t1,t2,t3,t4,t5,t6,t7] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t,t)   [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8)    = [t1,t2,t3,t4,t5,t6,t7,t8] ; {-# INLINE convert #-}
instance Convertible (t,t,t,t,t,t,t,t,t) [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8,t9) = [t1,t2,t3,t4,t5,t6,t7,t8,t9] ; {-# INLINE convert #-}


-- === Boool === --

-- TODO: TH vvv
type ToBool    a = Convertible  a Bool
type ToBool'   a = Convertible' a Bool
type FromBool  a = Convertible  Bool a
type FromBool' a = Convertible' Bool a
type IsBool    a = (ToBool  a, FromBool  a)
type IsBool'   a = (ToBool' a, FromBool' a)

toBool    :: ToBool    a => a -> Bool
toBool'   :: ToBool'   a => a -> Bool
fromBool  :: FromBool  a => Bool -> a
fromBool' :: FromBool' a => Bool -> a
asBool    :: IsBool    a => Iso' a Bool
asBool'   :: IsBool'   a => Iso' a Bool
toBool    = convert               ; {-# INLINE toBool    #-}
toBool'   = convert'              ; {-# INLINE toBool'   #-}
fromBool  = convert               ; {-# INLINE fromBool  #-}
fromBool' = convert'              ; {-# INLINE fromBool' #-}
asBool    = iso toBool  fromBool  ; {-# INLINE asBool    #-}
asBool'   = iso toBool' fromBool' ; {-# INLINE asBool'   #-}

