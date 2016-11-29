{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Convert.Base where

import Prelude
import Control.Lens
import qualified Data.Map as Map
import Data.Word
import Data.Bits (shiftR)


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

class MaybeConvertible a e b | a b -> e where
    tryConvert :: a -> Either e b

class MaybeConvertible' a e b | b -> a e where
    tryConvert' :: a -> Either e b

class Convertible a b where
    convert :: a -> b

-- Maybe we should change the name to Specializable and specialize?
class Convertible' a b | b -> a where
    convert' :: a -> b

type BiCastable a b = (Castable a b, Castable b a)

class Castable a b where
    cast :: a -> b
    default cast :: Convertible a b => a -> b
    cast = convert

type IsoMaybeConvertible  a e b = (MaybeConvertible  a e b, MaybeConvertible  b e a)
type IsoMaybeConvertible' a e b = (MaybeConvertible' a e b, MaybeConvertible' b e a)
type IsoConvertible       a b   = (Convertible       a b  , Convertible       b a  )
type IsoConvertible'      a b   = (Convertible'      a b  , Convertible'      b a  )
type IsoCastable          a b   = (Castable          a b  , Castable          b a  )

class ConvertibleM  m n where convertM  :: m t1 -> n t1
class ConvertibleM2 m n where convertM2 :: m t1 t2 -> n t1 t2
class ConvertibleM3 m n where convertM3 :: m t1 t2 t3 -> n t1 t2 t3
class ConvertibleM4 m n where convertM4 :: m t1 t2 t3 t4 -> n t1 t2 t3 t4
class ConvertibleM5 m n where convertM5 :: m t1 t2 t3 t4 t5 -> n t1 t2 t3 t4 t5

-- utils

unsafeConvert :: Show e => MaybeConvertible a e b => a -> b
unsafeConvert a =
    case tryConvert a of
      Left  e -> error $ show e
      Right r -> r

casted :: IsoCastable a b => Iso' a b
casted = iso cast cast

converted :: IsoConvertible a b => Iso' a b
converted = iso convert convert

-- instances

-- The following instances are commented out because they are pure EVIL.
-- Lets consider following situation - we've got an instance:
--     instance Castable (Edge src tgt) (Edge src' tgt') where ...
-- if we use it passing the same arguments it will overlap with the
--     instance Castable a a
-- in such way it ould be not possible to resolve by GHC!
--
instance {-# OVERLAPPABLE #-} Castable    a a where cast    = id ; {-# INLINE cast    #-}
-- instance {-# OVERLAPPABLE #-} Convertible a a where convert = id ; {-# INLINE convert #-}

instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Maybe a) (Maybe b) where convert = fmap convert ; {-# INLINE convert #-}


instance {-# OVERLAPPABLE #-}                  Castable [a] [a]  where cast = id        ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable a a' => Castable [a] [a'] where cast = fmap cast ; {-# INLINE cast #-}


instance Ord k => Convertible [(k, a)] (Map.Map k a) where
    convert = Map.fromList

instance Convertible (Map.Map k a) [(k, a)] where
    convert = Map.toList

instance Convertible ()                  [t] where convert _                            = []
instance Convertible (t,t)               [t] where convert (t1,t2)                      = [t1,t2]
instance Convertible (t,t,t)             [t] where convert (t1,t2,t3)                   = [t1,t2,t3]
instance Convertible (t,t,t,t)           [t] where convert (t1,t2,t3,t4)                = [t1,t2,t3,t4]
instance Convertible (t,t,t,t,t)         [t] where convert (t1,t2,t3,t4,t5)             = [t1,t2,t3,t4,t5]
instance Convertible (t,t,t,t,t,t)       [t] where convert (t1,t2,t3,t4,t5,t6)          = [t1,t2,t3,t4,t5,t6]
instance Convertible (t,t,t,t,t,t,t)     [t] where convert (t1,t2,t3,t4,t5,t6,t7)       = [t1,t2,t3,t4,t5,t6,t7]
instance Convertible (t,t,t,t,t,t,t,t)   [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8)    = [t1,t2,t3,t4,t5,t6,t7,t8]
instance Convertible (t,t,t,t,t,t,t,t,t) [t] where convert (t1,t2,t3,t4,t5,t6,t7,t8,t9) = [t1,t2,t3,t4,t5,t6,t7,t8,t9]


-- TODO: http://stackoverflow.com/questions/8350814/converting-64-bit-double-to-bytestring-efficiently
instance Convertible Word64 (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8) where
    convert word = (unpack 56, unpack 48, unpack 40, unpack 32, unpack 24, unpack 16, unpack 8, unpack 0)
        where unpack = fromIntegral . shiftR word
