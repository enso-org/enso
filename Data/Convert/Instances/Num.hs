{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Convert.Instances.Num where

import Data.Convert.Bound
import Data.Convert.Base
import Data.Monoid
import qualified Data.ByteString as BS
import GHC.Int
import Data.Word
import Data.Bits (shiftR)

intTypes   = [ Type ''Int      $ layoutBounds $ IntLayout Signed 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , Type ''Integer  $ layoutBounds $ InfiniteLayout
             , Type ''Int8     $ layoutBounds $ IntLayout Signed 8
             , Type ''Int16    $ layoutBounds $ IntLayout Signed 16
             , Type ''Int32    $ layoutBounds $ IntLayout Signed 32
             , Type ''Int64    $ layoutBounds $ IntLayout Signed 64
             ]

wordTypes  = [ Type ''Word     $ layoutBounds $ IntLayout Signed 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , Type ''Word8    $ layoutBounds $ IntLayout Signed 8
             , Type ''Word16   $ layoutBounds $ IntLayout Signed 16
             , Type ''Word32   $ layoutBounds $ IntLayout Signed 32
             , Type ''Word64   $ layoutBounds $ IntLayout Signed 64
             ]

floatTypes = [ Type ''Float    $ layoutBounds $ InfiniteLayout
             , Type ''Double   $ layoutBounds $ InfiniteLayout
             , Type ''Rational $ layoutBounds $ InfiniteLayout
             ]

charType = Type ''Char $ Bounds 0 1114111 -- max char enum is 1114111

integralTypes = intTypes <> wordTypes

-------------

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

-- ...

--------------

numConversions =  conversions [| fromIntegral            |] integralTypes (integralTypes <> floatTypes)
               <> conversions [| truncate                |] floatTypes    integralTypes
               <> conversions [| realToFrac              |] floatTypes    floatTypes
               <> conversions [| fromIntegral . fromEnum |] charType      (integralTypes <> floatTypes)
               <> conversions [| toEnum . fromIntegral   |] integralTypes charType
