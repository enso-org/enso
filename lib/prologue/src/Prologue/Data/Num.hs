{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Prologue.Data.Num (module Prologue.Data.Num, module X) where

import Prelude
import Prelude as X ( Int, Integer, Float, Double, Rational, Word
                    , Num ((+), (*), abs, signum, fromInteger, negate, (-))
                    , Real (toRational)
                    , Integral (quot, rem, div, mod, quotRem, divMod, toInteger)
                    , Fractional ((/), recip, fromRational)
                    , Floating (pi, exp, log, sqrt, (**), logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)
                    , RealFrac (properFraction, truncate, round, ceiling, floor)
                    , RealFloat (floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE, atan2)
                    , subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac
                    )

import Data.Int  as X (Int, Int8, Int16, Int32, Int64)
import Data.Word as X (Word, Word8, Word16, Word32, Word64)



isIntegral     :: RealFrac a =>        a -> Bool
isIntegralPrec :: RealFrac a => Int -> a -> Bool
isIntegral            = isIntegralPrec 10                                                      ; {-# INLINE isIntegral     #-}
isIntegralPrec prec a = round (10 ^ (fromIntegral prec) * (a - (fromIntegral $ round a))) == 0 ; {-# INLINE isIntegralPrec #-}
