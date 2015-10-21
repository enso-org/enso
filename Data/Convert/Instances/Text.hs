{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}


module Data.Convert.Instances.Text()
where

import Data.Convert.Base
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Word (Word8)
import Data.Foldable



instance Convertible TS.Text [Char] where
    {-# INLINE convert #-}
    convert = TS.unpack

instance Convertible TS.Text TL.Text where
    {-# INLINE convert #-}
    convert = TL.fromStrict

instance Convertible TS.Text TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.fromText

instance Convertible TS.Text BS.ByteString where
    {-# INLINE convert #-}
    convert = TE.encodeUtf8

instance Convertible TS.Text BL.ByteString where
    {-# INLINE convert #-}
    convert = BL.fromStrict . TE.encodeUtf8

instance Convertible TS.Text BB.Builder where
    {-# INLINE convert #-}
#if MIN_VERSION_text(1,2,0)
    convert = TE.encodeUtf8Builder
#else
    convert = convert . TE.encodeUtf8
#endif



instance Convertible TL.Text [Char] where
    {-# INLINE convert #-}
    convert = TL.unpack

instance Convertible TL.Text TS.Text where
    {-# INLINE convert #-}
    convert = TL.toStrict

instance Convertible TL.Text TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.fromLazyText

instance Convertible TL.Text BS.ByteString where
    {-# INLINE convert #-}
    convert = convert . TLE.encodeUtf8

instance Convertible TL.Text BL.ByteString where
    {-# INLINE convert #-}
    convert = TLE.encodeUtf8

instance Convertible TL.Text BB.Builder where
    {-# INLINE convert #-}
#if MIN_VERSION_text(1,2,0)
    convert = TLE.encodeUtf8Builder
#else
    convert = convert . TLE.encodeUtf8
#endif



instance Convertible TLB.Builder [Char] where
    {-# INLINE convert #-}
    convert = convert . TLB.toLazyText

instance Convertible TLB.Builder TS.Text where
    {-# INLINE convert #-}
    convert = convert . TLB.toLazyText

instance Convertible TLB.Builder TL.Text where
    {-# INLINE convert #-}
    convert = TLB.toLazyText

instance Convertible TLB.Builder BS.ByteString where
    {-# INLINE convert #-}
    convert = convert . TLB.toLazyText

instance Convertible TLB.Builder BL.ByteString where
    {-# INLINE convert #-}
    convert = convert . TLB.toLazyText

instance Convertible TLB.Builder BB.Builder where
    {-# INLINE convert #-}
    convert = convert . TLB.toLazyText



instance Convertible BS.ByteString [Word8] where
    {-# INLINE convert #-}
    convert = BS.unpack

instance Convertible BS.ByteString TS.Text where
    {-# INLINE convert #-}
    convert = TE.decodeUtf8

instance Convertible BS.ByteString TL.Text where
    {-# INLINE convert #-}
    convert = TL.fromStrict . convert

instance Convertible BS.ByteString TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.fromText . convert

instance Convertible BS.ByteString BL.ByteString where
    {-# INLINE convert #-}
    convert = BL.fromStrict

instance Convertible BS.ByteString BB.Builder where
    {-# INLINE convert #-}
    convert = BB.byteString



instance Convertible BL.ByteString [Word8] where
    {-# INLINE convert #-}
    convert = BL.unpack

instance Convertible BL.ByteString TS.Text where
    {-# INLINE convert #-}
    convert = TL.toStrict . convert

instance Convertible BL.ByteString TL.Text where
    {-# INLINE convert #-}
    convert = TLE.decodeUtf8

instance Convertible BL.ByteString TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.fromLazyText . convert

instance Convertible BL.ByteString BS.ByteString where
    {-# INLINE convert #-}
    convert = BL.toStrict

instance Convertible BL.ByteString BB.Builder where
    {-# INLINE convert #-}
    convert = BB.lazyByteString



instance Convertible [Char] TS.Text where
    {-# INLINE convert #-}
    convert = TS.pack

instance Convertible [Char] TL.Text where
    {-# INLINE convert #-}
    convert = TL.pack

instance Convertible [Char] TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.fromString



instance Convertible [Word8] BS.ByteString where
    {-# INLINE convert #-}
    convert = BS.pack

instance Convertible [Word8] BL.ByteString where
    {-# INLINE convert #-}
    convert = BL.pack

instance Convertible [Word8] BB.Builder where
    {-# INLINE convert #-}
    convert = foldMap BB.word8



instance Convertible Char TS.Text where
    {-# INLINE convert #-}
    convert = TS.singleton

instance Convertible Char TL.Text where
    {-# INLINE convert #-}
    convert = TL.singleton

instance Convertible Char TLB.Builder where
    {-# INLINE convert #-}
    convert = TLB.singleton



instance Convertible Word8 BS.ByteString where
    {-# INLINE convert #-}
    convert = BS.singleton

instance Convertible Word8 BL.ByteString where
    {-# INLINE convert #-}
    convert = BL.singleton

instance Convertible Word8 BB.Builder where
    {-# INLINE convert #-}
    convert = BB.word8
