{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Convert.Instances.Text where

import Prelude
import Data.Convert.Class

import           Data.Default
import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Builder as BB
import           Data.Word               (Word8)

import Data.Convert.Instances.ByteString ()


-- === Utils === --

type ToLazyText   a = Convertible a TL.Text
type ToText       a = Convertible a TS.Text
type FromLazyText a = Convertible TL.Text a
type FromText     a = Convertible TS.Text a
type IsText       a = BiConvertible TS.Text a
type IsLazyText   a = BiConvertible TL.Text a

toText   :: ToText   a => a -> TS.Text
fromText :: FromText a => TS.Text -> a
toText   = convert ; {-# INLINE toText   #-}
fromText = convert ; {-# INLINE fromText #-}

toLazyText   :: ToLazyText   a => a -> TL.Text
fromLazyText :: FromLazyText a => TL.Text -> a
toLazyText   = convert ; {-# INLINE toLazyText   #-}
fromLazyText = convert ; {-# INLINE fromLazyText #-}


-- === Intances === --

instance Default TS.Text     where def = convert ("" :: String) ; {-# INLINE def #-}
instance Default TL.Text     where def = convert ("" :: String) ; {-# INLINE def #-}
instance Default TLB.Builder where def = convert ("" :: String) ; {-# INLINE def #-}

instance Convertible TS.Text String        where convert = TS.unpack                     ; {-# INLINE convert #-}
instance Convertible TS.Text TL.Text       where convert = TL.fromStrict                 ; {-# INLINE convert #-}
instance Convertible TS.Text TLB.Builder   where convert = TLB.fromText                  ; {-# INLINE convert #-}
instance Convertible TS.Text BS.ByteString where convert = TE.encodeUtf8                 ; {-# INLINE convert #-}
instance Convertible TS.Text BL.ByteString where convert = BL.fromStrict . TE.encodeUtf8 ; {-# INLINE convert #-}
instance Convertible TS.Text BB.Builder    where convert = TE.encodeUtf8Builder          ; {-# INLINE convert #-}

instance Convertible TL.Text String        where convert = TL.unpack                     ; {-# INLINE convert #-}
instance Convertible TL.Text TS.Text       where convert = TL.toStrict                   ; {-# INLINE convert #-}
instance Convertible TL.Text TLB.Builder   where convert = TLB.fromLazyText              ; {-# INLINE convert #-}
instance Convertible TL.Text BS.ByteString where convert = convert . TLE.encodeUtf8      ; {-# INLINE convert #-}
instance Convertible TL.Text BL.ByteString where convert = TLE.encodeUtf8                ; {-# INLINE convert #-}
instance Convertible TL.Text BB.Builder    where convert = TLE.encodeUtf8Builder         ; {-# INLINE convert #-}

instance Convertible TLB.Builder String        where convert = convert . TLB.toLazyText  ; {-# INLINE convert #-}
instance Convertible TLB.Builder TS.Text       where convert = convert . TLB.toLazyText  ; {-# INLINE convert #-}
instance Convertible TLB.Builder TL.Text       where convert = TLB.toLazyText            ; {-# INLINE convert #-}
instance Convertible TLB.Builder BS.ByteString where convert = convert . TLB.toLazyText  ; {-# INLINE convert #-}
instance Convertible TLB.Builder BL.ByteString where convert = convert . TLB.toLazyText  ; {-# INLINE convert #-}
instance Convertible TLB.Builder BB.Builder    where convert = convert . TLB.toLazyText  ; {-# INLINE convert #-}

instance Convertible BS.ByteString [Word8]     where convert = BS.unpack               ; {-# INLINE convert #-}
instance Convertible BS.ByteString TS.Text     where convert = TE.decodeUtf8           ; {-# INLINE convert #-}
instance Convertible BS.ByteString TL.Text     where convert = TL.fromStrict . convert ; {-# INLINE convert #-}
instance Convertible BS.ByteString TLB.Builder where convert = TLB.fromText . convert  ; {-# INLINE convert #-}
instance Convertible BS.ByteString BB.Builder  where convert = BB.byteString           ; {-# INLINE convert #-}

instance Convertible BL.ByteString [Word8]     where convert = BL.unpack                  ; {-# INLINE convert #-}
instance Convertible BL.ByteString TS.Text     where convert = TL.toStrict . convert      ; {-# INLINE convert #-}
instance Convertible BL.ByteString TL.Text     where convert = TLE.decodeUtf8             ; {-# INLINE convert #-}
instance Convertible BL.ByteString TLB.Builder where convert = TLB.fromLazyText . convert ; {-# INLINE convert #-}
instance Convertible BL.ByteString BB.Builder  where convert = BB.lazyByteString          ; {-# INLINE convert #-}

instance Convertible String TS.Text     where convert = TS.pack        ; {-# INLINE convert #-}
instance Convertible String TL.Text     where convert = TL.pack        ; {-# INLINE convert #-}
instance Convertible String TLB.Builder where convert = TLB.fromString ; {-# INLINE convert #-}

instance Convertible [Word8] BS.ByteString where convert = BS.pack          ; {-# INLINE convert #-}
instance Convertible [Word8] BL.ByteString where convert = BL.pack          ; {-# INLINE convert #-}
instance Convertible [Word8] BB.Builder    where convert = foldMap BB.word8 ; {-# INLINE convert #-}

instance Convertible Char TS.Text     where convert = TS.singleton  ; {-# INLINE convert #-}
instance Convertible Char TL.Text     where convert = TL.singleton  ; {-# INLINE convert #-}
instance Convertible Char TLB.Builder where convert = TLB.singleton ; {-# INLINE convert #-}

instance Convertible Word8 BS.ByteString where convert = BS.singleton ; {-# INLINE convert #-}
instance Convertible Word8 BL.ByteString where convert = BL.singleton ; {-# INLINE convert #-}
instance Convertible Word8 BB.Builder    where convert = BB.word8     ; {-# INLINE convert #-}
