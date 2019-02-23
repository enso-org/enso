{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Convert.Instances.Num where

import Prelude

import Data.Convert.Class
import GHC.Int
import Data.Word
import Data.Bits (shiftR)

import Language.Haskell.TH
import Data.Default
import Control.Lens
import Data.Proxy
import Foreign.C.Types (CSize)


-- === Errors === --

data BoundError = BoundError deriving (Show)
instance Default BoundError where def = BoundError ; {-# INLINE def #-}


-- === Numeric types === --

data Sign = Signed | Unsigned deriving (Show, Eq)

data NumRange = BitRange Sign Integer
              | InfiniteRange
              deriving (Show, Eq)

data NumType = NumType { _name   :: Name
                       , _layout :: NumRange
                       } deriving (Show, Eq)
makeLenses ''NumType

intTypes :: [NumType]
intTypes   = [ NumType ''Int      $ BitRange Signed 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , NumType ''Integer  $ InfiniteRange
             , NumType ''Int8     $ BitRange Signed 8
             , NumType ''Int16    $ BitRange Signed 16
             , NumType ''Int32    $ BitRange Signed 32
             , NumType ''Int64    $ BitRange Signed 64
             ]

wordTypes :: [NumType]
wordTypes  = [ NumType ''Word     $ BitRange Unsigned 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , NumType ''Word8    $ BitRange Unsigned 8
             , NumType ''Word16   $ BitRange Unsigned 16
             , NumType ''Word32   $ BitRange Unsigned 32
             , NumType ''Word64   $ BitRange Unsigned 64
             ]

floatTypes :: [NumType]
floatTypes = [ NumType ''Float    $ InfiniteRange
             , NumType ''Double   $ InfiniteRange
             , NumType ''Rational $ InfiniteRange
             ]

integralTypes :: [NumType]
integralTypes = intTypes <> wordTypes


-- === Helpers === --

defNumConvertAssert :: forall r a. (Num a, Ord a, Bounded r, Integral r) => a -> Maybe BoundError
defNumConvertAssert = defConvertAssert (> fromIntegral (maxBound @r)) ; {-# INLINE defNumConvertAssert #-}

defNumConvertAssertP :: forall r a. (Num a, Ord a, Bounded r, Integral r) => Proxy r -> a -> Maybe BoundError
defNumConvertAssertP _ = defNumConvertAssert @r ; {-# INLINE defNumConvertAssertP #-}


-- === TH numeric conversions generators === --

mkConversion :: Q Exp -> NumType -> NumType -> Q Dec
mkConversion mf l r = do
    f <- mf
    let mkSafeConv    = InstanceD Nothing [] (AppT (AppT (ConT $ mkName "Convertible")        (ConT $ l ^. name)) (ConT $ r ^. name)) (inlinedFn "convert" f)
        mkPartialConv = InstanceD Nothing [] (AppT (AppT (ConT $ mkName "PartialConvertible") (ConT $ l ^. name)) (ConT $ r ^. name))
                      $ cerrTInst
                      : inlinedFn "unsafeConvert" f
                     <> inlinedFn "convertAssert" (AppE (VarE $ mkName "defNumConvertAssertP") (proxy $ r ^. name))
        cerrTInst     = TySynInstD (mkName "ConversionError") (TySynEqn [ConT $ l ^. name, ConT $ r ^. name] (ConT $ mkName "BoundError"))
        inlineAll n   = PragmaD (InlineP (mkName n) Inline FunLike AllPhases)
        inlinedFn n f = [ValD (VarP $ mkName n) (NormalB f) [], inlineAll n]
        proxy     n   = SigE (ConE $ mkName "Proxy") (AppT (ConT $ mkName "Proxy") (ConT n))
    return $ case (l ^. layout, r ^. layout) of
        (BitRange Unsigned lbit, BitRange Unsigned rbit) -> if rbit   >= lbit   then mkSafeConv else mkPartialConv
        (BitRange Signed   lbit, BitRange Signed   rbit) -> if rbit   >= lbit   then mkSafeConv else mkPartialConv
        (BitRange Unsigned lbit, BitRange Signed   rbit) -> if rbit   >= 2*lbit then mkSafeConv else mkPartialConv
        (BitRange Signed   lbit, BitRange Unsigned rbit) -> if 2*rbit >= lbit   then mkSafeConv else mkPartialConv
        (InfiniteRange         , BitRange _ _          ) -> mkPartialConv
        (BitRange _ _          , InfiniteRange         ) -> mkSafeConv
        (InfiniteRange         , InfiniteRange         ) -> mkSafeConv
{-# INLINE mkConversion #-}

mkConversions :: Q Exp -> [NumType] -> [NumType] -> Q [Dec]
mkConversions exp ls rs = sequence $ mkConversion exp <$> ls <*> rs ; {-# INLINE mkConversions #-}

conversions :: Q [Dec]
conversions = do
    t1 <- mkConversions [| fromIntegral |] integralTypes (integralTypes <> floatTypes)
    t2 <- mkConversions [| truncate     |] floatTypes    integralTypes
    t3 <- mkConversions [| realToFrac   |] floatTypes    floatTypes
    return $ t1 <> t2 <> t3
{-# INLINE conversions #-}


-- === Other instances === --

-- TODO: http://stackoverflow.com/questions/8350814/converting-64-bit-double-to-bytestring-efficiently
instance Convertible Word64 (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8) where
    convert word = (unpack 56, unpack 48, unpack 40, unpack 32, unpack 24, unpack 16, unpack 8, unpack 0)
        where unpack = fromIntegral . shiftR word
    {-# INLINE convert #-}

instance Convertible Int CSize where
    convert = fromIntegral ; {-# INLINE convert #-}
